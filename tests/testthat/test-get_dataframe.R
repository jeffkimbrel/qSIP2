test_that("correct dataframes are returned", {
  expect_snapshot(get_dataframe(example_feature_object))
  expect_snapshot(get_dataframe(example_sample_object))
  expect_snapshot(get_dataframe(example_source_object))
  expect_snapshot(get_dataframe(example_qsip_object, type = "feature"))
  expect_snapshot(get_dataframe(example_qsip_object, type = "sample"))
  expect_snapshot(get_dataframe(example_qsip_object, type = "source"))
})

test_that("unknown type gives error", {
  expect_error(get_dataframe(example_qsip_object, type = "features"), class = "qsip_invalid_argument")
  expect_error(get_dataframe(example_qsip_object, type = "samples"), class = "qsip_invalid_argument")
  expect_error(get_dataframe(example_qsip_object, type = "sources"), class = "qsip_invalid_argument")
})
