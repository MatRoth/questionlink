data <- questionlink_example_data #external data


test_that("validate_input_is.dataframe", {
  expect_error(validate_input("a"), "Data is not a dataframe.")
})

test_that("validate_input columns are present", {
  data_without_year <- subset(data, select = -c(year))

  expect_error(validate_input(data_without_year),
               "year")

  data_without_question <- subset(data, select = -c(question))

  expect_error(validate_input(data_without_question),
               "question")

  data_without_response <- subset(data, select = -c(response))

  expect_error(validate_input(data_without_response),
               "response")

  data_without_weight <- subset(data, select = -c(weight))


})

test_that("validate_input datatype of columns", {
  wrong_datatype <- data
  wrong_datatype$year <- as.character(wrong_datatype$year)
  expect_error(validate_input(wrong_datatype),
               "year")

  wrong_datatype <- data
  wrong_datatype$question <- 1:nrow(wrong_datatype)
  expect_error(validate_input(wrong_datatype),
               "question")

  wrong_datatype <- data
  wrong_datatype$response <- as.character(wrong_datatype$response)
  expect_error(validate_input(wrong_datatype),
               "response")

  wrong_datatype <- data
  wrong_datatype$weight <- as.character(wrong_datatype$weight)
  expect_error(validate_input(wrong_datatype),
               " weight")

  wrong_datatype <- data
  wrong_datatype$population <- 1:nrow(wrong_datatype)
  expect_error(validate_input(wrong_datatype),
               "population")
})

test_that("validate_input checks for NAs", {
  data$response[2] <- NA

  expect_error(validate_input(data),
               "The following columns contain NAs"
  )
})
