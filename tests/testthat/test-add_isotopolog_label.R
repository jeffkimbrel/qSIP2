test_that("works correctly", {
  expect_snapshot(add_isotopolog_label(example_source_df, isotope = "Isotope"))
})

test_that("Missing isotope column gives error", {
  expect_error(add_isotopolog_label(example_source_df, isotope = "not_found"),
               "not_found column not found")
})

