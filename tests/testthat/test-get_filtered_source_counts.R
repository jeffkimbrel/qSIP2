normal_qsip <- readRDS(test_path("fixtures", "qsip_normal_strict_EAF.rds"))

test_that("function works", {
  expect_snapshot(get_filtered_source_counts(normal_qsip))
})


test_that("wrong input gives errors", {
  expect_error(get_filtered_source_counts(example_qsip_object), "this function requires a qsip object that has been run through run_feature_filter()")
  expect_error(get_filtered_source_counts(example_feature_df), "qsip_data_object should be class <qsip_data>")
})
