test_that("Working as expected returns NULL", {
  expect_null(validate_abundances(example_feature_df, "ASV"))
})

test_that("Non-numeric data throws error", {
  testdf <- example_feature_df |>
    dplyr::mutate(STRING = "string")
  expect_error(validate_abundances(testdf, "ASV"))
})

test_that("Non-integer data throws error", {
  testdf <- example_feature_df |>
    dplyr::mutate(NUMERIC = `149_F1` / 2)
  expect_error(validate_abundances(testdf, "ASV"))
})

test_that("Negative value throws error", {
  testdf <- example_feature_df |>
    dplyr::mutate(`149_F1` = 0 - `149_F1`)
  expect_error(validate_abundances(testdf, "ASV"))
})
