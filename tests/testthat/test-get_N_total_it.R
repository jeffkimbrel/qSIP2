test_that("works with correct object", {
  expect_snapshot(get_N_total_it(example_qsip_growth_object, t = 0))
  expect_error(get_N_total_it(example_feature_df), "qsip_data_object should be class <qsip_data>")
})

test_that("invalid timepoints give error", {
  expect_error(get_N_total_it(example_qsip_growth_object, t = 333), "no source_mat_ids with a 't = 333' timepoint were found in <qsip_data>")
  expect_error(get_N_total_it(example_qsip_growth_object, t = -2), "no source_mat_ids with a 't = -2' timepoint were found in <qsip_data>")
  expect_error(get_N_total_it(example_qsip_growth_object, t = "ABC"), "t must be numeric")
})

test_that("works with grouping", {
  expect_snapshot(get_N_total_it(example_qsip_growth_object, t = 0, group = "isotopolog"))
  expect_error(get_N_total_it(example_qsip_growth_object, t = 0, group = "not_a_column"), "grouping variable not_a_column not found in source_data")
})



