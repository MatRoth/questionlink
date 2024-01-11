#' Prepare data for harmonization
#'
#' The function prepares the data for harmonization with Observed Score Equating
#' in a Random Groups design (OSE-RG) by finding all possible opportunities for harmonization in the data.
#'
#' @details
#'
#' OSE-RG can be used when data from two question measuring the same construct are
#' used in the same population at roughly similar times (e.g. the same year).
#'
#' The data has to be supplied as a tibble/data.frame in a "long format".
#' The data.frame/tibble has to have the following columns names and variable types:
#'  - year: Numeric variable which specifies the year in which the survey was conducted. Can not be of type Date or Factor. If other units of time are needed look here.
#'  - question: Character variable which specifies which kind of question was asked.
#'  - response: Numeric variable which contains the response values. NAs and missings should be removed.
#'              Note that the responses should be recoded so that higher values represent the same meaning on the scale.
#'  - (optional) weight: Numeric variable which contains the weight associated with a response.
#'  - (optional) population: Character variable which describes the population a response is attributed to.
#'                           Data should only be harmonized within one population. If no population is supplied
#'                           all data will be treated to originate from the same population.
#'
#' @param data A tibble/data.frame containing the data in a format outlined in the details below.
#' @param use_relay A logical indicating whether relay connections should be calculated. Default = FALSE.
#' @param allowed_direct_connections Further filtering of direct connections. Not documented at the moment.
#' @param allowed_relay_connections Further filtering of relay connections.   Not documented at the moment.
#' @param time_relaxation A numeric which specifies how much time is allowed to differ to establish direct connections. Default = 0.
#' @param remove_circular_relays Logical whether circular relays (relays from the same source question to the same target question
#'                               should be removed). Default is TRUE.
#' @param scale_min_max A named list which contains numeric vectors of length two, e.g. 'list("Question A" = c(0,5))'.
#'                      The name of the numeric vector should correspond to one
#'                      of the questions supplied in 'data'.The first element of
#'                      the numeric vector is the minimum of a scale,
#'                      the second element is the maximum of a scale.
#'                      Should be supplied if not all possible response options were used
#'                      by respondents. See Details for more information.
#' @return An object of type 'questionlink' 'questionlink_prepare'.
#' @export
ql_prepare <- function(data,
                       use_relay = F,
                       allowed_direct_connections = NULL,
                       allowed_relay_connections = NULL,
                       time_relaxation = 0,
                       remove_circular_relays = TRUE,
                       scale_min_max = list()){
  # input checks
  optional_col_check_result <- check_optional_cols(data)
  validate_input(optional_col_check_result$data)
  scale_min_max_check(data,scale_min_max)

  # find connections
  direct_connections <- optional_col_check_result$data |>
    find_direct_connections(time_relaxation) |>
    apply_custom_filter(allowed_direct_connections) # does nothing if arg is empty

  if(use_relay == T){
    relay_connections <- direct_connections |>
      find_relay_connections(remove_circular_relays,
                             time_relaxation) |>
      apply_custom_filter(allowed_relay_connections)

    connections <- dplyr::bind_rows(direct_connections,relay_connections) |>
      dplyr::relocate(source_question,.before = source_year) |> #enforce identical column order for source, relay and target column
      dplyr::relocate(relay_question, .before = relay_source_year) |>
      dplyr::relocate(target_question,.before = target_year)}
  else{
        connections <- direct_connections
      }

  # Add s3 object type
  add_s3_object_type_prepare(data = optional_col_check_result$data,
                             connections = dplyr::distinct(connections),
                             population_flag = optional_col_check_result$population_flag,
                             weight_flag = optional_col_check_result$weight_flag,
                             scale_min_max = scale_min_max,
                             remove_circular_relays_flag = remove_circular_relays)
}


#' Checks whether optional columns 'population' & 'weight' are supplied.
#' If not flags are set to F, if yes T. If F dummy columns are created with
#' population = "dummy" and weight = 1.
#' @noRd
check_optional_cols <- function(data){
  if(!("population" %in% names(data))){
    data[["population"]] <- "dummy"
    population_flag <- F}else{
      population_flag <- T
    }
  if(!("weight" %in% names(data))){
    data[["weight"]] <- 1
    weight_flag = F} else {
      weight_flag = T
    }
  list(data = data,
       population_flag = population_flag,
       weight_flag = weight_flag)
}



#' Validates, whether the supplied data follows specification and provides
#' helpful error messages if not.
#' @noRd

validate_input <- function(data){

  # 1. is.dataframe
  if(!is.data.frame(data)) stop(cli::format_error(c("x" = "Error: Data is not a dataframe.")))

  # 2 col names for mandatory columns
  if(length(setdiff(x = c("year","question", "response"),
                    y = names(data))) != 0){
    not_supplied_columns<- setdiff(x = c("year","question", "response"),
                                   y = names(data))
    stop(cli::format_error(c("i" = "The following columns are missing:", "x" = not_supplied_columns)))}


  # 3. datatypes positive checks
  expected_datatypes <- c(year = "numeric",
                          question = "character",
                          response = "numeric",
                          weight = "numeric",
                          population = "character") # named vector
  
  expected_datatypes <- expected_datatypes[names(expected_datatypes) %in% names(data)] # only check those columns which are in supplied data
  
  supplied_datatypes <- purrr::map(data,\(x) mode(x)) # gets datatypes of every column

  datatype_check <- supplied_datatypes[names(expected_datatypes)] == expected_datatypes

  if(!all(datatype_check)){
    false_columns <- names(datatype_check[datatype_check == F])
    expected_values <- expected_datatypes[false_columns]
    false_colums_expected_values <- paste(false_columns, ":", expected_values)
    cli::cli_abort(c(
      "The following columns have wrong datatypes:",
      "x" = "{false_columns}",
      "should have the following datatype:",
      "i" = "{false_colums_expected_values}"))
  }

  # 4. Extra check for year to be only int or dbl (not Date, factor, etc.)
  if(!(class(data$year) == "numeric" | class(data$year) == "integer")){
    cli::cli_abort(c(
      "The following column has wrong datatype:",
      "x" = "year",
      "i" = "The column year should have plain integers or doubles as its datatype. For more information, refer to the 'Arguments' section of the ql_prepare() documentation."
    ))
  }

  # 5. Check for NAs
  nas <- apply(data, 2, function(x) sum(is.na(x)))
  nas_colnames <- names(which(nas > 0))

  if (length(nas_colnames) > 0) {
    nas_message <- "The following columns contain NAs:"
    formated_message <-purrr::map_chr(nas_colnames,\(col){
      nas_count <- sum(is.na(data[, col]))
      paste(nas_message,col, ":", nas_count, "NAs.")
    }) |> setNames(nm=rep("x",length(nas_colnames)))

    stop(cli::format_error(c(formated_message, "i" = "Please check the dataset for any missing values.")))
  }

  # 6. Check if only full integer scores are found
  if(any(data$response %% 1 != 0)){
    cli::cli_abort(c(
      "Non-integer responses were found.",
      "x" = "response",
      "i" = "The column response should have full numbers as response options. For more information, refer to the 'Arguments' section of the ql_prepare() documentation."
    ))
  }

  return(invisible()) # if all tests passed
}


#' Checks if the questions supplied in scale_min_max are actually found in the data
#' @noRd
scale_min_max_check <- function(data,scale_min_max){
  questions <- unique(data$question)
  scale_min_max_names <- names(scale_min_max)

  # Check for duplications
  if(any(duplicated(scale_min_max_names))){
    duplicates <- scale_min_max_names[duplicated(scale_min_max_names)]
    cli::cli_abort(c(
      "x" = "Duplicate questions were supplied to scale_min_max: {.val {false_names}.",
      "i" = "Remove duplicate questions from scale_min_max"))
  }

  false_names <- setdiff(scale_min_max_names,questions)
  if(any(!(scale_min_max_names %in% questions))){
    cli::cli_abort(c(
      "x" = "Minimum and maximum for question(s) {.val {false_names}} were supplied
             via scale_min_max but the question(s) were not found in the data.",
      "i" = "Questions found in the data are:",
      paste(data$question |> unique(),
            collapse = ", ")))
  }
  return(invisible())
}

#' Finds all allowed direct connections and applies default filter
#' @noRd
find_direct_connections <- function(data,time_relaxation){

  #all possible combinations
  unique_elements <- data |>
    dplyr::select(year,question,population) |>
    unique() |>
    purrr::pmap_chr(paste,sep=",")

  all_possible_combinations <- dplyr::tibble(source = unique_elements,
                target = source)|>
    tidyr::expand(source,target) |>
    tidyr::separate(col = source,
                    into = c("source_year","source_question","source_population"),
                    sep = ",") |>
    tidyr::separate(col = target,
                    into = c("target_year","target_question","target_population"),
                    sep = ",") |>
    dplyr::mutate(dplyr::across(dplyr::contains("year"),as.numeric))

  # default filter (no connection to same question, only within a population and year)
  default_direct_connections <- all_possible_combinations |>
    dplyr::filter(source_question != target_question,
                  abs(source_year-target_year) <= time_relaxation,
                  source_population == target_population) |>
    dplyr::mutate(connection_type = dplyr::if_else(source_year == target_year,
                                                   "direct",
                                                   "time_relaxation"))

  return(default_direct_connections)
}

#' Applies custom filtering to the data if supplied by the user.
#'
#' If no filter is specified by the user, the unchanged data is returned.
#' @noRd
apply_custom_filter <- function(data,filter_statements){
  filter_statement <- rlang::enquos(filter_statements)

  # check if argument is empty
  if(is.null(filter_statements)) return(data)
  if(rlang::quo_is_null(filter_statements[[1]])) return(data)

  # iteratively applies filter statements
  filtered_data<- purrr::reduce(
    .x = filter_statements,
    .f = \(current_data,filter_statement) dplyr::filter(current_data,rlang::eval_tidy(filter_statement,data = current_data)),
    .init = data)

  return(filtered_data)
}

#' Finds all allowed relay connections
#'
#' If no relays connections are requested by the user, the unchanged data is returned.
#' @noRd
find_relay_connections <- function(direct_connections,remove_circular_relays,time_relaxation){
  # Find relay connections
  relay_source<- direct_connections |>
    dplyr::select(dplyr::contains("target")) |>
    setNames(paste("relay_source",
                   c("year","question","population"),
                   sep="_"))
  relay_target <- direct_connections |>
    dplyr::select(dplyr::contains("source")) |>
    setNames(paste("relay_target",
                   c("year","question","population"),
                   sep="_"))

  relay_connections <- dplyr::left_join(relay_source,relay_target,
                                 by = c("relay_source_question" = "relay_target_question")) |>
    dplyr::rename(relay_question = relay_source_question) |>
    dplyr::filter(
      relay_source_population == relay_target_population) |>
    dplyr::left_join(direct_connections,                    # Add target data
              by = c("relay_question" = "source_question",
                     "relay_target_population" = "source_population",
                     "relay_target_year" = "source_year"))|>
    dplyr::mutate(connection_type = "relay")
  relay_connections <- direct_connections |>
    dplyr::select(-connection_type) |>
    dplyr::rename(relay_source_year = target_year,
                  relay_source_population = target_population,
                  relay_question = target_question) |>
    dplyr::left_join(relay_connections, # Add source data
                     by = c("relay_source_year", "relay_question", "relay_source_population")) |>
    dplyr::filter(
      abs(source_year-relay_source_year) <= time_relaxation,   # Apply default filter
      abs(relay_target_year-target_year) <= time_relaxation,
      abs(relay_source_year-relay_target_year) > time_relaxation)

  if(remove_circular_relays == T){
    relay_connections <- relay_connections |>
      dplyr::filter(source_question != target_question)
  }

  return(relay_connections)
}

#' Prepares the S3 object with the current state of
#' the questionlink workflow ("stage_prepare")
#' @noRd
add_s3_object_type_prepare <- function(data,connections,population_flag,weight_flag,scale_min_max,remove_circular_relays_flag){
  ql_object <- structure("questionlink_object",
            class = c("questionlink","questionlink_prepare"),
            connections = connections,
            data = data,
            population_flag = population_flag,
            weight_flag = weight_flag,
            scale_min_max = scale_min_max,
            remove_circular_relay_flag = remove_circular_relays_flag)
  return(ql_object)
}


