qsip_normal_strict_filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_filtered.rds"))
qsip_normal_strict_resampled <- readRDS(test_path("fixtures", "qsip_normal_strict_resampled.rds"))

test_that("no errors given", {
  expect_snapshot(get_resample_data(qsip_normal_strict_resampled))
  expect_snapshot(get_resample_data(qsip_normal_strict_resampled, type = "labeled"))
  expect_snapshot(get_resample_data(qsip_normal_strict_resampled, type = "unlabeled"))
  expect_snapshot(get_resample_data(qsip_normal_strict_resampled, type = "labeled", pivot = TRUE))
})

test_that("non-resampled objects give error", {
  expect_error(get_resample_data(example_qsip_object), "This function requires a qsip object that has been run through run_resampling()")
  expect_error(get_resample_data(qsip_normal_strict_filtered), "This function requires a qsip object that has been run through run_resampling()")
})

test_that("wrong type given", {
  expect_error(get_resample_data(qsip_normal_strict_resampled, type = "not_a_valid_type"), "type must be one of 'all', 'unlabeled', or 'labeled'")
})

test_that("pivot not boolean", {
  expect_error(get_resample_data(qsip_normal_strict_resampled, pivot = "yes"), "pivot must be TRUE or FALSE")
})
