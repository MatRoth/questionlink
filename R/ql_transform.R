#' Transform data using questionlink recoding tables
#'
#' Transforms data using recoding tables obtained by using `ql_prepare() |> ql_harmoninze()`.
#'
#' This function is step 3 of 3 in the questionlink harmonization workflow.
#' This function either transforms data which is supplied in the harmonization workflow, or transforms
#' new data, which can be added via the "new_data" argument.
#'

#' @param ql_harmonize_object An object of type questionlink_harmonize, created by `ql_harmonize()`
#' @param target_question A character value specifying the question which serves as the transformation target.
#' @param new_data A data frame or tibble which contains the following columns "question", "response". The data frame
#' also needs to contain the column "population", if the column was also used in the `ql_prepare()` step.
#'
#' @return An object of type `questionlink` `questionlink_harmonize`
#'
#' @details
#' ## Handling of circular relays in `ql_transform()`:
#'
#' If circular relays are set to TRUE (default is FALSE) during the invocation of `ql_prepare()`, harmonizations from
#' question A to question A contain the numerical equivalents that were found using the relays. If no circular relays are used, the
#' results of `ql_transformed` contain the same response options for the target questions as were supplied in the data for question A to question A. 


#' @export
ql_transform <- function(ql_harmonize_object,target_question,new_data){ # questionlink_harmonize -> questionlink_transform

  #validation
  if(!("questionlink_harmonize" %in% class(ql_harmonize_object))){
    cli::cli_abort(c(x = "ql_transform() expects an object of type 'questionlink_harmonize' for argument 'ql_harmonize_object'.",
                     i = "Use ql_harmonize() to create an object of type 'questionlink_harmonize'."))
  }

  # Check if arguments conform to specification. The following checks are valid for
  # both new_data and no new_data.
  if(missing(target_question)) cli::cli_abort(c(
    "x" = "Argument {.code target_question} is missing.",
    "i" = "Please supply a target question as a transformation target.",
    "Possible target questions are:",
    paste(attr(ql_harmonize_object,"data")$question |> unique(),
          collapse = ", ")))

  if(!(target_question %in% attr(ql_harmonize_object,"data")$question |> unique())){
    cli::cli_abort(c(
      "x" = "Target question {.val {target_question}} not found in recoding database.",
      "i" = "Please supply a target question as a transformation target.",
      "Possible target questions are:",
      paste(attr(ql_harmonize_object,"data")$question |> unique(),
            collapse = ", ")))
  }

  selected_recoding_info <- attr(ql_harmonize_object,"summarized_rec_tbl") |>
    dplyr::rename(rec_target_question = target_question) |>
    dplyr::filter(rec_target_question == target_question) # returns summarized_rec_tbl if target question == NULL

  if(missing(new_data)){

    join_vars <- c("response" = "source","question" = "source_question","population" = "population")

    data_to_transform  <- attr(ql_harmonize_object,"data")

    transformed_data <- dplyr::left_join(x = data_to_transform,
                                         y = selected_recoding_info,
                                         by = join_vars) |>
      dplyr::mutate(median_equivalent = dplyr::if_else(question == target_question,response,median_equivalent))

    if(attr(ql_harmonize_object,"population_flag") == F){
      tansformed_data <- transformed_data |> dplyr::select(-population)
    }

    if(attr(ql_harmonize_object,"weight_flag") == F){
      tansformed_data <- transformed_data |> dplyr::select(-weight)
    }

    if(attr(ql_harmonize_object,"population_flag") == F){
      transformed_data <- transformed_data |> dplyr::select(-population)}

    # check if transformed data contains NAs
    if(any(is.na(transformed_data$median_equivalent))){
      missing_entries <- transformed_data |>
        dplyr::filter(is.na(median_equivalent)) |>
        dplyr::select(dplyr::any_of(c("question","population","response"))) |>
        dplyr::distinct() |>
        purrr::pmap_chr(\(...){
          dots <- list(...)
          if(attr(ql_harmonize_object,"population_flag") == T){
          paste("Source question:",
                dots$question,
                "| Population:",
                dots$population,
                "| Response option:",
                dots$response)
          } else {
            paste("Source question:",
                  dots$question,
                  "| Response option:",
                  dots$response)
          }
        }) |>
        sort()


      cli::cli_warn(c(
        "i" = "Transformed data contains responses with no numerical equivalent in the recoding database.\n",
        "The following entries have no numerical equivalent:\n",
        cli_bullet(missing_entries,"*")))

    }
  }

  if(!missing(new_data)){
    # Check whether data follows specification
    if(!is.data.frame(new_data)){
      cli::cli_abort(c(
      "x" = "{.code new_data} is not a data.frame",
      "i" = "Please supply {.code new_data} as a data.frame."))
      }

    if(!("response" %in% names(new_data))){
      cli::cli_abort(c(
      "x" = "{.code new_data} does not contain a column named 'response'",
      "i" = "Please supply {.code new_data} as a data.frame."))
      }

    if(!("source_question" %in% names(new_data))){
      cli::cli_abort(c(
      "x" = "{.code new_data} does not contain a column named 'source_question'",
      "i" = "Please supply {.code new_data} as a data.frame."))
      }

    if(!(
      all(
        unique(new_data$source_question) %in% unique(attr(ql_harmonize_object,"summarized_rec_tbl")$source_question)))){

      cli::cli_abort(c(
        "x" = "{.code new_data} contains source questions not found in recoding database:",
        "*" = paste(setdiff(unique(new_data$source_question),
                      unique(attr(ql_harmonize_object,"summarized_rec_tbl")$source_question)),
              collapse = ", "),
        "i" = "The following source questions are available in the recoding database:",
        setNames(paste(unique(attr(ql_harmonize_object,"summarized_rec_tbl")$source_question)),
                 nm = rep("*",
                          length(unique(attr(ql_harmonize_object,"summarized_rec_tbl")$source_question))))))
    }

    if(attr(ql_harmonize_object,"population_flag") == T){
      if(!(
        all(
          unique(new_data$population) %in% unique(attr(ql_harmonize_object,"summarized_rec_tbl")$population)))){
        cli::cli_abort(c(
          "x" = "{.code new_data} contains populations not found in recoding database:",
          "*" = paste(setdiff(unique(new_data$population),
                              unique(attr(ql_harmonize_object,"summarized_rec_tbl")$population)),
                      collapse = ", "),
          "i" = "The following populations are available in the recoding database:",
          setNames(unique(attr(ql_harmonize_object,"summarized_rec_tbl")$population),
                   nm = rep("*",length(unique(attr(ql_harmonize_object,"summarized_rec_tbl")$population))))))
      }
    }

    if(!(is.numeric(new_data$response[1]))) cli::cli_abort(c(
      "x" = "Column response in {.code new_data} is not of type 'numeric' ",
      "i" = "Please supply numeric values in the column 'response' in {.code new_data}"))


    # harmonization of new_data with population flag == T
    if(attr(ql_harmonize_object,"population_flag") == T){
      if(!("population" %in% names(new_data))){
        cli::cli_abort(c(
          "x" = "Population information missing.",
          "i" = "Please add a column 'population'.",
          "Population values found in questionlink harmonization object:",
          paste(selected_recoding_info$population |> unique(),
                collapse = ", ")))
      }
      transformed_data <- dplyr::left_join(x = new_data,
                                           y = selected_recoding_info,
                                           by = c("source_question" = "source_question",
                                                  "response" = "source",
                                                  "population" = "population"))
    } else {

      transformed_data <- dplyr::left_join(x = new_data,
                                           y = selected_recoding_info,
                                           by = c("source_question" = "source_question",
                                                  "response" = "source")) |>
        dplyr::select(-population)}
    # check if transformed new_data contains NAs
    if(any(is.na(transformed_data$median_equivalent))){
      missing_entries <- transformed_data |>
        dplyr::filter(is.na(median_equivalent)) |>
        dplyr::select(dplyr::any_of(c("question","population","response"))) |>
        dplyr::distinct() |>
        purrr::pmap_chr(\(...){
          dots <- list(...)
          paste("Source question:",
                dots$question,
                "| Population:",
                dots$population,
                "| Response option:",
                dots$response)
        }) |>
        sort()

      cli::cli_warn(c(
        "i" = "{.code new_data} contains responses with no numerical equivalent in the recoding database.\n",
        "The following entries have no numerical equivalent:\n",
        cli_bullet(missing_entries,"*")))
    }
  }


  transformed_data <- transformed_data |>
    dplyr::rename(target_question = rec_target_question)

  transformed_data
}

# Small helper function to dynamically create cli lists with vectors
#' @noRd
cli_bullet <- function(vec,bullet_type){
  setNames(vec,rep(bullet_type,length(vec)))
}
