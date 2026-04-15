test_that("works with correct object", {
  expect_snapshot(get_N_total_it(example_qsip_growth_object, t = 0))
  expect_error(get_N_total_it(example_feature_df), class = "qsip_wrong_class")
})

test_that("invalid timepoints give error", {
  expect_error(get_N_total_it(example_qsip_growth_object, t = 333), class = "qsip_invalid_argument")
  expect_error(get_N_total_it(example_qsip_growth_object, t = -2), class = "qsip_invalid_argument")
  expect_error(get_N_total_it(example_qsip_growth_object, t = "ABC"), "t must be numeric")
})

test_that("works with grouping", {
  expect_snapshot(get_N_total_it(example_qsip_growth_object, t = 0, group = "isotopolog"))
  expect_error(get_N_total_it(example_qsip_growth_object, t = 0, group = "not_a_column"), class = "qsip_column_not_found")
})



