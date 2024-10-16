qsip_test = example_feature_object
qsip_test@type = "normalized"

test_that("runs correctly", {
  expect_snapshot(calculate_tube_rel_abundance(example_source_object, example_sample_object, example_feature_object))
})

test_that("make sure data types are correct", {
  expect_error(calculate_tube_rel_abundance(example_source_df,
                                            example_sample_object,
                                            example_feature_object),
               "source_data must be of class <qsip_source_data>")
  expect_error(calculate_tube_rel_abundance(example_source_object,
                                            example_sample_df,
                                            example_feature_object),
               "sample_data must be of class <qsip_sample_data>")
  expect_error(calculate_tube_rel_abundance(example_source_object,
                                            example_sample_object,
                                            example_feature_df),
               "feature_data must be of class <qsip_feature_data>")
})
