
#' Print information on ql_prepare object
#'
#' Displays progress in the questionlink workflow and a highlevel overview
#' over the number of found connections.
#'
#' @param x An object of type "questionlink_prepare"
#'
#' @export
print.questionlink_prepare <- function(x,...){
  # calculated values
  distinct_question_pairs <- attributes(x)$connections |>
    dplyr::select(source_question,target_question) |>
    dplyr::distinct() |>
    nrow()
  n_direct_connections <- attributes(x)$connections |>
    dplyr::filter(connection_type == "direct") |>
    nrow()
  n_time_relaxation_connections <- attributes(x)$connections |>
    dplyr::filter(connection_type == "time_relaxation")|>
    nrow()
  n_relay_connections <- attributes(x)$connections |>
    dplyr::filter(connection_type == "relay")|>
    nrow()


  # print to command line
  cli::cli_h1("QuestionLink Object Type: QuestionLink Prepare")
  cli::cli_par()
  cli::cli_text("Current stage of the QuestionLink workflow: {.strong ql_prepare ✔} -> ql_harmonize -> ql_transform")
  cli::cli_end()
  cli::cli_par()
  cli::cli_li("Number of unique question combinations: {.val {distinct_question_pairs}}")
  cli::cli_h2("Number of found connections:")
  cli::cli_li("Direct connections: {.val {n_direct_connections}}")
  cli::cli_li("Time relaxation connections: {.val {n_time_relaxation_connections}}")
  cli::cli_li("Relay connections: {.val {n_relay_connections}}")
  cli::cli_end()
  cli::cli_h2("Additional information:")
  if(attr(x,"population_flag")){
    populations <- attr(x,"data")$population |> unique()
    cli::cli_li("Populations found: {.val {populations}}")
  } else{
    cli::cli_li("No population variable supplied.\nData is interpreted as comming from one population.")
  }
  if(attr(x,"weight_flag")) cli::cli_li("Weights were found") else cli::cli_li("No weight variable was supplied.")
  cli::cli_end()
  cli::cli_par()
  cli::cli_text("For more information on the connections use summary().")
  cli::cli_end()
  invisible(x)
}

#' Summary function, printing full list connections
#'
#' @export
summary.questionlink_prepare <- function(object,...){
  # calculated values
  distinct_question_pairs <- attributes(object)$connections |>
    dplyr::select(source_question,target_question) |>
    dplyr::distinct() |>
    nrow()
  n_direct_connections <- attributes(object)$connections |>
    dplyr::filter(connection_type == "direct") |>
    nrow()
  n_time_relaxation_connections <- attributes(object)$connections |>
    dplyr::filter(connection_type == "time_relaxation")|>
    nrow()
  n_relay_connections <- attributes(object)$connections |>
    dplyr::filter(connection_type == "relay")|>
    nrow()

  summary_data <- attributes(object)$connections |>
    dplyr::group_by(source_question,target_question,connection_type) |>
    dplyr::summarize(n = dplyr::n()) |>
    tidyr::pivot_wider(names_from = connection_type,
                       values_from = n) |>
    dplyr::ungroup()
  # check whether connection types are missing
  #browser()
  missing_connection_type <- setdiff(x = c("direct","time_relaxation","relay"),
                                     y = intersect(names(summary_data),
                                                   c("direct","time_relaxation","relay")))
  if(length(missing_connection_type) != 0){
    summary_data[,missing_connection_type] <- 0
  }

  summary_data <- summary_data |>
    dplyr::select('Source question' = source_question,
                  'Target question' = target_question,
                  'Direct' = direct,
                  'Time relaxation' = time_relaxation,
                  'Relay' = relay) |>
    dplyr::arrange(dplyr::desc(Direct))

  question_information <- attr(object,"data") |>
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

  if(!all(attr(object,"data")$population == "dummy")){
    question_information <- question_information |>
      dplyr::transmute(question = paste(question,population,sep = "|"),
                       question_information)
  }

  if(length(attr(object,"scale_min_max")) > 0){
  user_supplied_min_max <- dplyr::tibble(question = names(attr(object,"scale_min_max")),
                                         nmbr_supplied_resp_opts = purrr::map(
                                           attr(object,"scale_min_max"),
                                           \(cur_resp){
                                             dplyr::tibble(response = cur_resp[1]:cur_resp[2])
                                           }))
  response_options <- question_information |>
    tidyr::unnest(question_information) |>
    dplyr::mutate(question_raw = stringr::str_match(question,"^(.*)\\|")[,2])|>
    dplyr::left_join(user_supplied_min_max,
                     by = c("question_raw" = "question"),
                     relationship = "many-to-one")|>
    dplyr::transmute("Question" = question,
                     "Response options" = purrr::map2(.x = nmbr_supplied_resp_opts,
                                                      .y = nmbr_found_resp_opts,
                                                      \(supplied,found){
                                                        if(is.null(supplied)){
                                                          found
                                                        } else {
                                                          supplied
                                                        }
                                                      }),
                     "User supplied" = !purrr::map_lgl(nmbr_supplied_resp_opts,is.null))
  } else {
    response_options <- question_information |>
      tidyr::unnest(question_information)|>
      dplyr::transmute("Question" = question,
                "Response options" = nmbr_found_resp_opts,
                "User supplied" = FALSE)
  }


  #print to command line
  cli::cli_h1("Summary: QuestionLink Prepare")
  cli::cli_par()
  cli::cli_text("Current stage of the QuestionLink workflow: {.strong ql_prepare ✔} -> ql_harmonize -> ql_transform")
  cli::cli_end()
  cli::cli_par()
  cli::cli_li("Number of unique question combinations: {.val {distinct_question_pairs}}")
  cli::cli_h2("Number of found connections:")
  cli::cli_li("Direct connections: {.val {n_direct_connections}}")
  cli::cli_li("Time relaxation connections: {.val {n_time_relaxation_connections}}")
  cli::cli_li("Relay connections: {.val {n_relay_connections}}")
  cli::cli_h2("Additional information:")
  if(attr(object,"population_flag")){
    populations <- attr(object,"data")$population |> unique()
    cli::cli_li("Populations found: {.val {populations}}")
  } else{
    cli::cli_li("No population variable supplied.\nData is interpreted as comming from one population.")
  }
  if(attr(object,"weight_flag")) cli::cli_li("Weights were found") else cli::cli_li("No weight variable was supplied.")
  cli::cli_end()
  cli::cli_h2("Overview over question combinations:")
  knitr::kable(summary_data,format = "simple") |> print()
  cli::cli_text("\n")
  cli::cli_h2("Overview over response options found:")
  knitr::kable(response_options,format = "simple") |> print()
  invisible(object)
}


#' Plot function plotting found connections between questions
#' @param x A questionlink prepare object.
#' @param major_breaks_divisor Divisor for major breaks of the x-axis.
#' @param minor_breaks_divisor Divisor for minor breaks of the x-axis.
#' @param combine_population_plots How to handle plotting if multiple populations are supplied.
#' @export
plot.questionlink_prepare <- function(x,
                                      major_breaks_divisor = 10,
                                      minor_breaks_divisor = 1,
                                      combine_population_plots = T){

  if(attr(x,"population_flag") == F){
    plot<- generate_plot(connections = x |> attr("connections"),
                  data = x |> attr("data"),
                  major_breaks_divisor,
                  minor_breaks_divisor)
  } else {
    nested_connections <- x |>
      attr("connections") |>
      dplyr::mutate(population = source_population)|>
      dplyr::group_by(population) |>
      tidyr::nest(.key = "connections")

    nested_data <- x |>
      attr("data") |>
      dplyr::group_by(population) |>
      tidyr::nest(.key = "data")

    plots <- nested_connections |>
      dplyr::left_join(nested_data,by = "population")|>
      purrr::pmap(\(...){
        dots <- list(...)
        cur_pop <- dots$population
        cur_con <- dots$connections
        cur_dat <- dots$data
        generate_plot(cur_con,
                      cur_dat,
                      major_breaks_divisor,
                      minor_breaks_divisor)+
          ggplot2::labs(title = ggplot2::element_blank(),
                        subtitle = paste("Population:",cur_pop))
      })
    #Set title


    if(combine_population_plots == T){
       plots[[1]] <- plots[[1]]+ ggplot2::labs(title = "Question by years")
       #Align axis
       plots <- purrr::map(plots,\(cur_plot){
         suppressMessages(
         cur_plot + ggplot2::xlim(c(min(attr(x,"data")$year),
                                    max(attr(x,"data")$year))))})
       plot <- patchwork::wrap_plots(plots,ncol=1,guides = "collect") &
         ggplot2::theme(legend.position = 'bottom')
    } else {
      plot <- plots|>purrr::map(\(cur_plot) cur_plot+ggplot2::labs(title = "Question by years"))
    }
  }
  return(plot)
}

generate_plot <- function(connections,data,major_breaks_divisor,minor_breaks_divisor){

  # Determin the number of different questions in the harmonization data
  number_of_questions <- data |>
    dplyr::pull(question) |>
    unique() |>
    length()

  #Determin to how many other question each question connects
  question_connections_table <- connections |>
    dplyr::select(source_question, target_question) |>
    dplyr::distinct() |>
    dplyr::arrange(source_question) |>
    dplyr::group_by(source_question) |>
    dplyr::count() |>
    dplyr::mutate(
      pct_link = 100 * n / (number_of_questions -1),
      question_connections = dplyr::case_when(
        pct_link == 0 ~ "to none",
        pct_link == 100 ~ "to all",
        pct_link > 0 & pct_link < 100 ~ "to some"
      ),
      question_connections = factor(
        question_connections,
        levels = c("to none", "to some", "to all")
      )
    ) |>
    dplyr::select(question = source_question, question_connections)

  # Question Year Data for plotting with added connections data
  question_year_connections_df <- data |>
    dplyr::group_by(year, question) |>
    dplyr::summarise() |>
    dplyr::arrange(year, question) |>
    dplyr::full_join(question_connections_table, by = join_by(question))




  # Plot scale dimension
  year_min = min(question_year_connections_df$year)
  year_max = max(question_year_connections_df$year)
  years_vec = c(year_min:year_max)
  major_breaks_vec = years_vec[years_vec %% major_breaks_divisor == 0]
  minor_breaks_vec = years_vec[years_vec %% minor_breaks_divisor == 0]

  # Generate plot
  question_year_connections_df |>
    dplyr::arrange(question) |>
    dplyr::mutate(question = factor(question),
                  question = `levels<-`(question,rev(levels(question)))) |>
    ggplot2::ggplot(ggplot2::aes(year, question, color = question_connections, shape = question_connections))+
    ggplot2::geom_point(size = 2, stroke = 1.5)+
    ggplot2::scale_x_continuous(breaks = major_breaks_vec,
                                minor_breaks = years_vec)+
    ggplot2::scale_color_manual(values = c("darkred", "orange", "darkgreen"), drop = FALSE)+
    ggplot2::scale_shape_manual(values = c(4, 1, 19), drop = FALSE)+
    ggplot2::theme_minimal(base_size = 16)+
    ggplot2::guides(shape = ggplot2::guide_legend("Question is connected:"),
                    color = ggplot2::guide_legend("Question is connected:"))+
    ggplot2::theme(
      legend.position = "top",
      panel.grid.major.y = element_line(size = 2))+
    ggplot2::labs(title = "Question by years",
                  x = "Year",
                  y = "Question")
}



#' Print information on ql_harmonize object
#'
#' Displays progress in the questionlink workflow and a highlevel overview
#' over the number of found connections.
#'
#' @param x An object of type "questionlink_harmonize"
#'
#' @export
print.questionlink_harmonize <- function(x,...){
  # calculated values
  distinct_question_pairs <- attributes(x)$connections |>
    dplyr::select(source_question,target_question) |>
    dplyr::distinct() |>
    nrow()
  n_direct_connections <- attributes(x)$connections |>
    dplyr::filter(connection_type == "direct") |>
    nrow()
  n_time_relaxation_connections <- attributes(x)$connections |>
    dplyr::filter(connection_type == "time_relaxation")|>
    nrow()
  n_relay_connections <- attributes(x)$connections |>
    dplyr::filter(connection_type == "relay")|>
    nrow()


  # print to command line
  cli::cli_h1("QuestionLink Object Type: QuestionLink Prepare")
  cli::cli_par()
  cli::cli_text("Current stage of the QuestionLink workflow: {.strong ql_prepare ✔} -> {.strong ql_harmonize ✔} -> ql_transform")
  cli::cli_end()
  cli::cli_par()
  cli::cli_li("Number of unique question combinations: {.val {distinct_question_pairs}}")
  cli::cli_h2("Number of found connections:")
  cli::cli_li("Direct connections: {.val {n_direct_connections}}")
  cli::cli_li("Time relaxation connections: {.val {n_time_relaxation_connections}}")
  cli::cli_li("Relay connections: {.val {n_relay_connections}}")
  cli::cli_end()
  cli::cli_h2("Additional information:")
  if(attr(x,"population_flag")){
    populations <- attr(x,"data")$population |> unique()
    cli::cli_li("Populations found: {.val {populations}}")
  } else{
    cli::cli_li("No population variable supplied.\nData is interpreted as comming from one population.")
  }
  if(attr(x,"weight_flag")) cli::cli_li("Weights were found") else cli::cli_li("No weight variable was supplied.")
  cli::cli_end()
  cli::cli_par()
  cli::cli_text("For more information on the connections use summary().")
  cli::cli_end()
  invisible(x)
}

#' Summary function, printing full list connections
#'
#' @export
summary.questionlink_harmonize <- function(object,...){
  # calculated values
  distinct_question_pairs <- attributes(object)$connections |>
    dplyr::select(source_question,target_question) |>
    dplyr::distinct() |>
    nrow()
  n_direct_connections <- attributes(object)$connections |>
    dplyr::filter(connection_type == "direct") |>
    nrow()
  n_time_relaxation_connections <- attributes(object)$connections |>
    dplyr::filter(connection_type == "time_relaxation")|>
    nrow()
  n_relay_connections <- attributes(object)$connections |>
    dplyr::filter(connection_type == "relay")|>
    nrow()

  summary_data <- attributes(object)$connections |>
    dplyr::group_by(source_question,target_question,connection_type) |>
    dplyr::summarize(n = dplyr::n()) |>
    tidyr::pivot_wider(names_from = connection_type,
                       values_from = n) |>
    dplyr::ungroup()
  # check whether connection types are missing
  #browser()
  missing_connection_type <- setdiff(x = c("direct","time_relaxation","relay"),
                                     y = intersect(names(summary_data),
                                                   c("direct","time_relaxation","relay")))
  if(length(missing_connection_type) != 0){
    summary_data[,missing_connection_type] <- 0
  }

  summary_data <- summary_data |>
    dplyr::select('Source question' = source_question,
                  'Target question' = target_question,
                  'Direct' = direct,
                  'Time relaxation' = time_relaxation,
                  'Relay' = relay) |>
    dplyr::arrange(dplyr::desc(Direct))

  #print to command line
  cli::cli_h1("Summary: QuestionLink Prepare")
  cli::cli_par()
  cli::cli_text("Current stage of the QuestionLink workflow: {.strong ql_prepare ✔} -> ql_harmonize -> ql_transform")
  cli::cli_end()
  cli::cli_par()
  cli::cli_li("Number of unique question combinations: {.val {distinct_question_pairs}}")
  cli::cli_h2("Number of found connections:")
  cli::cli_li("Direct connections: {.val {n_direct_connections}}")
  cli::cli_li("Time relaxation connections: {.val {n_time_relaxation_connections}}")
  cli::cli_li("Relay connections: {.val {n_relay_connections}}")
  cli::cli_h2("Additional information:")
  if(attr(object,"population_flag")){
    populations <- attr(object,"data")$population |> unique()
    cli::cli_li("Populations found: {.val {populations}}")
  } else{
    cli::cli_li("No population variable supplied.\nData is interpreted as comming from one population.")
  }
  if(attr(object,"weight_flag")) cli::cli_li("Weights were found") else cli::cli_li("No weight variable was supplied.")
  cli::cli_end()
  cli::cli_h2("Overview over questions:")
  knitr::kable(summary_data,format = "simple")
}
