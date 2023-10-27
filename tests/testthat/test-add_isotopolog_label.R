test_that("works correctly", {
  expect_snapshot(add_isotopolog_label(example_source_df, isotope = "Isotope"))
})

test_that("Missing isotope column gives error", {
  expect_error(add_isotopolog_label(example_source_df, isotope = "not_found"),
               "ERROR: Please provide a valid column name for the isotope data")
})
