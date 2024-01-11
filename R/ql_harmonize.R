#' Harmonize ql_prepare object
#'
#' Harmonizes all harmonization opportunities that were found with 'ql_prepare()' by creating recoding
#' tables for each harmonization opportunity.
#'
#' This function is step 2 of 3 in the questionlink harmonization workflow.
#'
#' Depending on the number of harmonization opportunities, this may take some time.
#'
#' @param ql_prepare_object An object of type questionlink_prepare, created by 'ql_prepare()'
#'
#' @return An object of type 'questionlink' 'questionlink_harmonize'
#'
#' @examples
#' ql_prepare(data) |> ql_harmonize()

#' @export
ql_harmonize <- function(ql_prepare_object){ # questionlink_prepare -> questionlink_harmonize

  stopifnot("Function argument must be of type questionlink_prepare" = "questionlink_prepare" %in% class(ql_prepare_object))

  # data pre-processing
  freq_tables <- get_frequency_tables(raw_data = attributes(ql_prepare_object)$data) |>
    dplyr::mutate(data = purrr::map2(data,
                                     question,
                                     \(cur_frq_tab,cur_question) enforce_full(cur_frq_tab,
                                                                 cur_question,
                                                                 attr(ql_prepare_object,"scale_min_max"))))

  #creation of hashtables
  char_id_num_id_hashtab <- hashtab(size = nrow(freq_tables))
  purrr::walk2(freq_tables$char_id,freq_tables$id,\(key,value){
    sethash(char_id_num_id_hashtab,key,value)})

  num_id_data_hashtab <- hashtab(size = nrow(freq_tables))
  purrr::walk2(freq_tables$id,freq_tables$data,\(key,value){
    sethash(num_id_data_hashtab,key,value)})

  recoding_tables_hashtab <- hashtab(size = nrow(attributes(ql_prepare_object)$connections))

  # Main harmonization pipeline
  connections_id <- convert_connections_to_id(attributes(ql_prepare_object)$connections,char_id_num_id_hashtab)
  # Fill recoding_tables_hashtab with recoding_tables
  purrr::walk(connections_id,
              \(cur_connection) mem_equate(cur_connection,
                                            num_id_data_hashtab,
                                            recoding_tables_hashtab,
                                            harmonize_equip_R),.progress = "Performing equating")
  recoding_table_tibbles <- purrr::map(connections_id,
                                               \(cur_connection) gethash(recoding_tables_hashtab,key = paste(cur_connection,collapse ="_"))) |>
    purrr::map(set_col_names)

  add_s3_object_type_harmonized(ql_prepare_object,recoding_table_tibbles)
}

get_frequency_tables <- function(raw_data){

  frequency_tables <- raw_data |>
    dplyr::group_by(question,year,population,response) |>
    dplyr::summarize(freq_resp_option = sum(weight)) |>
    dplyr::ungroup() |>
    dplyr::group_by(question,year,population) |>
    dplyr::mutate(cum_rel_freq = cumsum(freq_resp_option)/sum(freq_resp_option)) |>
    tidyr::nest() |>
    dplyr::ungroup()
  #adding numeric and character ids
  frequency_tables |>
    dplyr::mutate(id = as.double(1:nrow(frequency_tables)),
                  char_id = paste(question,year,population,sep = "_"))


}
standardize_frq_table <- function(frq_tab){
  frq_tab |>
    enforce_full() |>
    enforce_min()
}

enforce_full <- function(frq_tab,question,scale_min_max){ # tibble -> tibble
  scores <- frq_tab$response |> sort()
  if((length(scale_min_max) > 0) & (question %in% names(scale_min_max))){
    min_src <- scale_min_max[[question]][1]
    max_src <- scale_min_max[[question]][2]
  } else{
  min_src <- min(scores)
  max_src <- max(scores)
}
  range_src <- min_src:max_src
  full_scores <- tibble::tibble(response = min_src:max_src)

  if(nrow(frq_tab) != nrow(full_scores)){
    frq_tab <- dplyr::left_join(full_scores,frq_tab,by = "response")|>
      dplyr::mutate(freq = dplyr::if_else(is.na(freq_resp_option),0.0000001,freq_resp_option),
                    rel_freq = freq/sum(freq),
                    cum_rel_freq = cumsum(rel_freq)) |>
      tidyr::fill(dplyr::everything())

  }
  frq_tab
}
convert_connections_to_id <- function(connections,char_id_num_id_hashtab){
  connections |>
    purrr::transpose() |>
    purrr::map(\(cur_connection){ #hard-coded way to split df into correct chunks

      if(cur_connection$connection_type == "direct"||cur_connection$connection_type == "time_relaxation"){

        source_id <- gethash(char_id_num_id_hashtab,paste(cur_connection$source_question,
                                                          cur_connection$source_year,
                                                          cur_connection$source_population,
                                                          sep="_"))
        target_id <- gethash(char_id_num_id_hashtab,paste(cur_connection$target_question,
                                                          cur_connection$target_year,
                                                          cur_connection$target_population,
                                                          sep="_"))
        return(c(source_id,target_id))
      }
      if(cur_connection$connection_type == "relay"){
        source_id <- gethash(char_id_num_id_hashtab,paste(cur_connection$source_question,
                                                          cur_connection$source_year,
                                                          cur_connection$source_population,
                                                          sep="_"))
        relay_source_id <- gethash(char_id_num_id_hashtab,paste(cur_connection$relay_question,
                                                                cur_connection$relay_source_year,
                                                                cur_connection$relay_source_population,
                                                                sep="_"))
        relay_target_id <- gethash(char_id_num_id_hashtab,paste(cur_connection$relay_question,
                                                                cur_connection$relay_target_year,
                                                                cur_connection$relay_target_population,
                                                                sep="_"))
        target_id <- gethash(char_id_num_id_hashtab,paste(cur_connection$target_question,
                                                          cur_connection$target_year,
                                                          cur_connection$target_population,
                                                          sep="_"))
        return(c(source_id, relay_source_id, relay_target_id ,target_id))
      }
    },.progress = "Mapping ids to chunks.")
}

mem_equate <- function(cur_row,id_data_hashtab,equating_tables_hashtab,harm_fct){ # outputs nothing, mutates the equating_table_hashtab
  if(is.list(gethash(equating_tables_hashtab,as.character(paste(cur_row[1:(length(cur_row)-2)],collapse = "_")),"NA"))==F){ # Check if recoding table is in hashtable
    if(length(cur_row) == 2){ #base case for recursion: first direct equating
      cur_source_ftab <- gethash(id_data_hashtab,cur_row[1])
      cur_target_ftab <- gethash(id_data_hashtab,cur_row[2])
      cur_equating_result <- harm_fct(x = cur_source_ftab$response-min(cur_source_ftab$response), #built in standardization to zero
                                      src_cum_rel_freq = cur_source_ftab$cum_rel_freq,
                                      trgt_response = cur_target_ftab$response-min(cur_target_ftab$response),
                                      trgt_cum_rel_freq = cur_target_ftab$cum_rel_freq)+min(cur_target_ftab$response)
      new_hashed_result <- list(cur_source_ftab$response,cur_equating_result)
      equating_tables_hashtab[[as.character(paste(cur_row,collapse = "_"))]] <- new_hashed_result
    }else{ # if connection is relay connection but first equating step is not done
      mem_equate(cur_row[1:(length(cur_row)-2)],id_data_hashtab,equating_tables_hashtab,harm_fct) #do first step
      mem_equate(cur_row,id_data_hashtab,equating_tables_hashtab,harm_fct) # call again -> will go to relay steps
    }
  } else{ #relay steps
    cur_source_ftab <- gethash(id_data_hashtab,cur_row[[length(cur_row)-1]])
    cur_target_ftab <- gethash(id_data_hashtab,cur_row[[length(cur_row)]])
    hashed_result <- equating_tables_hashtab[[paste(cur_row[1:(length(cur_row)-2)],collapse = "_")]]
    cur_equating_result <- harm_fct(x = hashed_result[[length(hashed_result)]]-min(cur_source_ftab$response), #built in standardization to zero
                                    src_cum_rel_freq = cur_source_ftab$cum_rel_freq,
                                    trgt_response = cur_target_ftab$response-min(cur_target_ftab$response),
                                    trgt_cum_rel_freq = cur_target_ftab$cum_rel_freq)+min(cur_target_ftab$response)
    hashed_result[[length(hashed_result)+1]] <- cur_equating_result
    equating_tables_hashtab[[as.character(paste(cur_row,collapse = "_"))]] <- hashed_result
  }
}



# Equipercentile equating functions #----


get_P_from_x <- function(x, cum_rel_freq){ # num_vec -> num_vec
  # Given a continuous score, interpolates corresponding pecentile rank from supplied freqtab
  x_int <- trunc(x+0.5) #nearest integer
  F_hi <- cum_rel_freq[x_int+1]
  F_low  <- cum_rel_freq[ifelse(x_int>0,x_int,NA)]
  F_low[is.na(F_low)] <- 0

  100*(F_low + ((x-(x_int-0.5)) * (F_hi-F_low)))
}

get_x_from_P <- function(P, response,cum_rel_freq){ # num_vec -> num_vec
  # Given pecentile rank, interpolates corresponding continuous score from supplied freqtab
  xu_int <- purrr::map_dbl(P,\(P_cur) response[cum_rel_freq >P_cur/100 | dplyr::near(cum_rel_freq,P_cur/100)][1])
  F_low <- cum_rel_freq[ifelse(xu_int>0,xu_int,NA)]
  F_low[is.na(F_low)] <- 0
  F_hi  <- cum_rel_freq[xu_int+1]

  ((P/100-F_low)/(F_hi-F_low))+(xu_int-0.5)
}


harmonize_equip_R <- function(x,src_cum_rel_freq,trgt_response,trgt_cum_rel_freq){
  get_P_from_x(x,src_cum_rel_freq) |> get_x_from_P(trgt_response,trgt_cum_rel_freq)}

#' Prepares the S3 object with the current state of
#' the questionlink workflow ("stage_prepare")
#' @noRd
add_s3_object_type_harmonized <- function(ql_prepare_object,recoding_tables){
  # combine connections and recoding tables
  connect_rec_tbl <- attributes(ql_prepare_object)$connections |>
    dplyr::mutate(recoding_tables)

  # combine recoding tables per question combination
  summarized_rec_tbl <- connect_rec_tbl |>
    tidyr::unnest(recoding_tables) |>
    dplyr::group_by(source_question,target_question,source_population,source) |>
    dplyr::summarize(median_equivalent = median(target)) |>
    dplyr::ungroup() |>
    dplyr::rename(population = source_population)

  if(attr(ql_prepare_object,"remove_circular_relay_flag") == T){
    self_equating <- summarized_rec_tbl |>
      dplyr::select(-c(target_question,median_equivalent)) |>
      dplyr::distinct() |>
      dplyr::mutate(target_question = source_question,
                    median_equivalent = source)
    summarized_rec_tbl <- dplyr::bind_rows(summarized_rec_tbl,
                                           self_equating)
  }

  ql_object <- structure("questionlink_object",
                         class = c("questionlink","questionlink_harmonize"),
                         connections_rec_tbl = connect_rec_tbl,
                         summarized_rec_tbl = summarized_rec_tbl,
                         data = attr(ql_prepare_object,"data"),
                         population_flag = attr(ql_prepare_object,"population_flag"),
                         weight_flag = attr(ql_prepare_object,"weight_flag"),
                         scale_min_max = attr(ql_prepare_object,"scale_min_max"),
                         remove_circular_relays_flag = attr(ql_prepare_object,"remove_circular_relays_flag"))
  return(ql_object)
}


#' Turns list of harmonization information into tibble with source (relay) target
#' column names.
#' @noRd
set_col_names <- function(chunk){ # list -> tibble
  if(length(chunk) == 2){
    attributes(chunk) <- list(class=c("tbl_df","tbl","data.frame"),
                              names = c("source","target"),
                              row.names = 1:length(chunk[[1]]))
  }else{
    attributes(chunk) <- list(class = c("tbl_df","tbl","data.frame"),
                              names = c("source","relay","target"),
                              row.names = 1:length(chunk[[1]]))}
  chunk
}
