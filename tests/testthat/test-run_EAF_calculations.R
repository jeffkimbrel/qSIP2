qsip_normal_strict_resampled <- readRDS(test_path("fixtures", "qsip_normal_strict_resampled.rds"))

test_that("works correctly", {
  expect_snapshot(run_EAF_calculations(qsip_normal_strict_resampled))
})

test_that("wrong type or not run through pre-steps", {
  expect_error(run_EAF_calculations(example_feature_object),
               "object must be a <qsip_data> object, not <qsip_feature_data>")
  expect_error(run_EAF_calculations(example_qsip_object))
})
