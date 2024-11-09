normal_qsip <- readRDS(test_path("fixtures", "qsip_normal_strict_EAF.rds"))

test_that("function works", {
  expect_snapshot(get_filtered_source_counts(normal_qsip))
})


test_that("wrong input gives errors", {
  expect_error(get_filtered_source_counts(example_qsip_object), "object is a non-filtered <qsip_data> object")
  expect_error(get_filtered_source_counts(example_feature_df), "object must be a <qsip_data> object, not <tbl_df>")
})
