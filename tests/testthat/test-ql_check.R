sample_data <- questionlink_example_data
test_that("Snapshot-tests of ql_check outputs",{
#is_singular test -> Checks whether multiple versions of the same question are present
 expect_snapshot(ql_prepare(sample_data |>
                              dplyr::bind_rows(dplyr::tibble(question = "A",
                                                             response = 100,
                                                             year = 1980,
                                                             weight = 1))) |>
                   ql_check())

  # is_continuous test
  expect_snapshot(ql_prepare(sample_data |>
                               dplyr::filter(question == "A",response != 3))  |>
                    ql_check())

  # has_negative test
  expect_snapshot(ql_prepare(sample_data |>
                               dplyr::bind_rows(dplyr::tibble(question = "A",
                                                              response = -1,
                                                              year = 2018,
                                                              weight = 1))) |>
                    ql_check())
})

