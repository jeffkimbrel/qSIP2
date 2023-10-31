test_that("works as expected", {
  expect_snapshot(find_shared_ids(example_source_object, example_sample_object, example_feature_object))
})
