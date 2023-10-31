test_that("runs correctly", {
  expect_snapshot(calculate_tube_rel_abundance(example_source_object, example_sample_object, example_feature_object))
})

test_that("make sure data types are correct", {
  expect_error(calculate_tube_rel_abundance(example_source_df,
                                            example_sample_object,
                                            example_feature_object),
               "ERROR: source_data must be of type qsip_source_data")
  expect_error(calculate_tube_rel_abundance(example_source_object,
                                            example_sample_df,
                                            example_feature_object),
               "ERROR: sample_data must be of type qsip_sample_data")
  expect_error(calculate_tube_rel_abundance(example_source_object,
                                            example_sample_object,
                                            example_feature_df),
               "ERROR: feature_data must be of type qsip_feature_data")
})
