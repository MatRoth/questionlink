#' Checks the supplied data for common pitfalls in the harmonization process
#'
#' ql_check() performs plausbility checks on the supplied data. It prints messages
#' if inconsistencies in the data are found which may influence the results of the harmonization.
#' In particular, three checks are performed:
#'
#' @section Check if multiple versions of the same question are found:
#' Each question should be associated with one set of response options. If a
#' one set of response options was found for one year (e.g. 1,2,3,4 in the year 2000) and another set was
#' found in another year (e.g. 1,2,3 in  the year 2004) there may be a problem with the data, such as missing cases
#' or falsly exculded cases. Make sure to include only one version of the question, as the harmonization may lead to wrong
#' results.
#'
#' @section Check if negative responses are found:
#' Negative values are used to represent missing values of some sort in most survey data files.
#' Check whether negative values are valid responses in your datafile (and ignore the message if they are valid).
#'
#' @section Check if response options are not used in a scale:
#' Missing response options (e.g. in a 4 point scale 1,2,4, response option 3 would be missing), can lead to
#' false harmonization results. If response options are missing, check if responses were falsely excluded from the
#' dataset. This check utilizes the information supplied by the 'scale_min_max' argument in ql_prepare().
#' If scale_min_max is left empty, the minimum and maximum found in the data will be used.

#' @export
ql_check <- function(ql_prepare_object){
  #validation
  if(!("questionlink_prepare" %in% class(ql_prepare_object))){
    cli::cli_abort(c(x = "ql_check expects an object of type 'questionlink_prepare'.",
                     i = "Use ql_prepare() to create an object of type 'questionlink_prepare'."))
  }



  question_information <- attr(ql_prepare_object,"data") |>
    dplyr::distinct(question,year,population,response) |>
    dplyr::group_by(question,year,population) |>
    tidyr::nest() |>
    dplyr::ungroup() |>
    dplyr::mutate(data = purrr::map(data,
                                    \(tbl) dplyr::arrange(tbl,response)))|>
    dplyr::arrange(question,year) |>
    dplyr::group_by(question,population,nmbr_found_resp_opts=data) |>
    tidyr::nest(.key = "years_question") |>
    dplyr::ungroup() |>
    dplyr::group_by(question,population) |>
    tidyr::nest(.key = "question_information") |>
    dplyr::ungroup()

  # Add scale_min_max information
  question_information <- question_information |>
    dplyr::mutate(scale_min_max = purrr::map_if(.x = question,
                                                .p = \(q) q %in% names(attr(ql_prepare_object,"scale_min_max")),
                                                .f = \(q) attr(ql_prepare_object,"scale_min_max")[q],
                                                .else = \(q) list()))

  # Remove dummy population information if no population was supplied
  if(!all(attr(ql_prepare_object,"data")$population == "dummy")){
    question_information <- question_information |>
      dplyr::transmute(question = paste(question,population,sep = "|"),
                       question_information,
                       scale_min_max)
  }



  check_result<- purrr::pmap(
    list(question_information$question,
         question_information$question_information,
         question_information$scale_min_max),
              check_questions)

  question_information |>
    dplyr::mutate(check_result) |>
    print_messages()

  return(invisible(ql_prepare_object))
}

#' Iterate through all questions|populations and gather messages and descriptives
#' @noRd
check_questions <- function(cur_q,cur_inf,scale_min_max){

  # Singular check
  is_singular_flag <- nrow(cur_inf) == 1

  # Continuous check
  if(length(scale_min_max) == 0 & is.list(scale_min_max)){
    is_continuous_flag <- cur_inf$nmbr_found_resp_opts |>
      purrr::map_lgl(\(cur_resp)
                     !any(cur_resp$response[-1]-(cur_resp$response[-length(cur_resp$response)])!=1))}
  else{
    is_continuous_flag <- cur_inf$nmbr_found_resp_opts |>
      purrr::map_lgl(\(cur_resp){
        length(setdiff(scale_min_max[[1]][1]:scale_min_max[[1]][2],cur_resp$response))==0
      })
  }

  # No negative check
  has_no_negative_flag <- cur_inf$nmbr_found_resp_opts |>
    purrr::map_lgl(\(cur_resp) !any(cur_resp$response < 0))

  list(is_singular = is_singular_flag,
       is_continuous_flag = is_continuous_flag,
       has_no_negative_flag = has_no_negative_flag)
}

#' Print messages of problems found by check_questions
#' @noRd
print_messages <- function(question_information){ # Add explainer lines before or after the check output is generated
  purrr::pmap(question_information,\(...){
    dots <- list(...)
    cur_q <- dots$question
    cur_q_inf <- dots$question_information
    cur_check_res <- dots$check_result
    cur_scale_min_max <- dots$scale_min_max
    if(length(cur_scale_min_max)>0){
      minimum <- paste0(" (",cur_scale_min_max[[1]][1],") ")
      maximum <- paste0(" (",cur_scale_min_max[[1]][2],") ")
    } else {
      minimum <- " "
      maximum <- " "
    }

    if(all(purrr::flatten_lgl(cur_check_res))){
      cli::cli_h2("{cur_q} ✔")
      return(invisible())
    }
    # Name instrument
    cli::cli_h2("{cur_q}")
    #is_singular message
    if(any(!cur_check_res$is_singular)){
      cli::cli_text("{.strong Issue:} Mutliple Versions of {cur_q} were found.")
      print_is_singular(cur_q,cur_q_inf,cur_check_res[1])
    }

    #is_continuous message
    if(any(!cur_check_res$is_continuous_flag)){
      cli::cli_text("{.strong Issue:} Not all response options between the minimum{minimum}and maximum{maximum}of {cur_q} were used.")
      print_is_continuous(cur_q,cur_q_inf,cur_check_res$is_continuous_flag)
    }


    #has_no_negative message
    if(any(!cur_check_res$has_no_negative_flag)){
      cli::cli_text("{.strong Issue:} Negative responses were found for {cur_q}.:")
      print_has_no_negative(cur_q,cur_q_inf,cur_check_res$has_no_negative_flag)
    }


  })
}

#' Prints the warning whether more than one version of the question is found
#' @noRd
print_is_singular <- function(question,question_information,is_singular_flag){
  if(is_singular_flag == T) return()

  purrr::pwalk(list(n = 1:nrow(question_information),
             question_information$nmbr_found_resp_opts,
             question_information$years_question),
        \(n,cur_resp_opt,cur_years){
          cli::cli_text(paste("Version",n))
          cli::cli_li(paste("Has the following response options:",paste(cur_resp_opt$response,collapse=", ")))
          cli::cli_li(paste("Occures in the following years:",paste(cur_years$year,collapse=", ")))
        }
      )

  cli::cli_alert_info("Ensure that each question has only one
                      version by defining a minimum and maximum of a
                      scale with the scale_min_max argument in
                      ql_prepare() or by only including one version of the question.",wrap = T)
  cli::cli_text("")
}

#' Prints the warning whether all response options between min and max were found
#' @noRd
print_is_continuous <- function(question,question_information,is_continuous_flag){
  if(any(!is_continuous_flag) == T){
    year <- question_information$years_question[!is_continuous_flag] |>
      dplyr::bind_rows() |>
      dplyr::pull(year) |>
      paste(collapse = ", ")
    cli::cli_text("Year(s): {year}")
    cli::cli_text("\n")
  }else{
    return()
  }
}

#' Prints the warning whether negative values were found.
#' @noRd
print_has_no_negative <- function(question,question_information,has_no_negative_flag){
  if(any(!has_no_negative_flag) == T){
    year <- question_information$years_question[!has_no_negative_flag] |>
      dplyr::bind_rows() |>
      dplyr::pull(year) |>
      paste(collapse = ", ")
    cli::cli_text("Year(s): {year}")
  }
}

