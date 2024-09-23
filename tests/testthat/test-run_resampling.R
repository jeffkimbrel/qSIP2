qsip_normal_strict_filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_filtered.rds"))

test_that("works correctly", {
  expect_snapshot(run_resampling(qsip_normal_strict_filtered, resamples = 100, with_seed = 21))
  expect_snapshot(run_resampling(qsip_normal_strict_filtered, resamples = 10))
  expect_snapshot(run_resampling(qsip_normal_strict_filtered, resamples = 10, progress = FALSE))
  expect_snapshot(run_resampling(qsip_normal_strict_filtered, resamples = 10, allow_failures = TRUE))
})

test_that("wrong input types fail", {
  expect_error(run_resampling(example_feature_object), "qsip_data_object should be class <qsip_data>")
  expect_error(run_resampling(example_qsip_object), "this function requires a qsip object that has been run through run_feature_filter()")
  expect_error(run_resampling(qsip_normal_strict_filtered, resamples = 0), "resamples should be positive")
  expect_error(run_resampling(qsip_normal_strict_filtered, resamples = "not_a_number"), "resamples should be class <numeric>")
  expect_error(run_resampling(qsip_normal_strict_filtered, progress = "not_an_option"), "progress must be either TRUE of FALSE")
})

test_that("same seed gives same result", {
  expect_identical(run_resampling(qsip_normal_strict_filtered, resamples = 10, with_seed = 22),
                   run_resampling(qsip_normal_strict_filtered, resamples = 10, with_seed = 22))
})

test_that("resample number is correct", {
  expect_equal(length(run_resampling(qsip_normal_strict_filtered, resamples = 10, with_seed = 22)@resamples$u), 10)
  expect_equal(length(run_resampling(qsip_normal_strict_filtered, resamples = 10, with_seed = 22)@resamples$l), 10)})

