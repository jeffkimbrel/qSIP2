test_that("Working as expected returns NULL", {
  expect_null(validate_abundances(example_feature_df, "ASV", type = "counts"))
  expect_null(validate_abundances(example_feature_df, "ASV", type = "coverage"))
  expect_null(validate_abundances(example_feature_df, "ASV", type = "normalized"))
})

test_that("Non-numeric data throws error", {
  testdf1 <- example_feature_df |>
    dplyr::mutate(STRING = "string")
  expect_error(validate_abundances(testdf1, "ASV", type = "counts"))
  expect_error(validate_abundances(testdf1, "ASV", type = "coverage"))
  expect_error(validate_abundances(testdf1, "ASV", type = "normalized"))

  # currently throwing a different error, but still passes checks here
  expect_error(validate_abundances(testdf1, "ASV", type = "relative"))
})

test_that("Non-integer data throws error, but not if type = coverage or relative", {
  testdf2 = example_feature_df |>
    tidyr::pivot_longer(cols = dplyr::where(is.numeric)) |>
    dplyr::group_by(name) |>
    dplyr::mutate(value = (.5*value) / sum(value)) |>
    tidyr::pivot_wider()
  expect_error(validate_abundances(testdf2, "ASV", type = "counts"))
  expect_null(validate_abundances(testdf2, "ASV", type = "coverage"))
  expect_null(validate_abundances(testdf2, "ASV", type = "relative"))
  expect_null(validate_abundances(testdf2, "ASV", type = "normalized"))
})

test_that("Negative value throws error", {
  testdf3 <- example_feature_df |>
    dplyr::mutate(`149_F1` = 0 - `149_F1`)
  expect_error(validate_abundances(testdf3, "ASV", type = "counts"))
  expect_error(validate_abundances(testdf3, "ASV", type = "coverage"))
  expect_error(validate_abundances(testdf3, "ASV", type = "relative"))
  expect_error(validate_abundances(testdf3, "ASV", type = "normalized"))
})


test_that("Relative abundance sums are equal to or less than 1", {
  testdf4 = example_feature_df |>
    tidyr::pivot_longer(cols = dplyr::where(is.numeric)) |>
    dplyr::group_by(name) |>
    dplyr::mutate(value = (2*value) / sum(value)) |>
    tidyr::pivot_wider()

  expect_error(validate_abundances(testdf4, "ASV", type = "relative"))
})

test_that("Type is not known", {
  expect_error(validate_abundances(example_feature_df, "ASV", type = "not_a_type"),
  "feature data type should be 'counts', 'coverage', 'normalized' or 'relative'")
})

