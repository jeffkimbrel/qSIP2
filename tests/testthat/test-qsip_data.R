test_that("works as expected", {
  expect_snapshot(qsip_data(
    example_source_object,
    example_sample_object,
    example_feature_object
  ))
  expect_true("qsip_data" %in% class(qsip_data(
    example_source_object,
    example_sample_object,
    example_feature_object
  )))
})

test_that("wrong input types fail", {
  expect_error(qsip_data(
    example_source_df,
    example_sample_object,
    example_feature_object
  ), "source_data should be of class <qsip_source_data>")
  expect_error(qsip_data(
    example_source_object,
    example_sample_df,
    example_feature_object
  ), "sample_data should be of class <qsip_sample_data>")
  expect_error(qsip_data(
    example_source_object,
    example_sample_object,
    example_feature_df
  ), "feature_data should be of class <qsip_feature_data>")
})
