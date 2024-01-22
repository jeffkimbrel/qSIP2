test_qsip <- example_qsip_object |>
  run_feature_filter(
    unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
    labeled_source_mat_ids = c("S178", "S179", "S180"),
    min_unlabeled_sources = 6,
    min_labeled_sources = 3,
    min_unlabeled_fractions = 6,
    min_labeled_fractions = 6
  )

test_that("works correctly", {
  expect_snapshot(run_resampling(test_qsip, resamples = 100, with_seed = 21))
})

test_that("wrong input types fail", {
  expect_error(run_resampling(example_feature_object), "qsip_data_object should be class <qsip_data>")
  expect_error(run_resampling(example_qsip_object), "this function requires a qsip object that has been run through run_feature_filter()")
  expect_error(run_resampling(test_qsip, resamples = 0), "resamples should be positive")
  expect_error(run_resampling(test_qsip, resamples = "not_a_number"), "resamples should be class <numeric>")
  expect_error(run_resampling(test_qsip, progress = "not_an_option"), "progress must be either TRUE of FALSE")
})

test_that("same seed gives same result", {
  expect_identical(run_resampling(test_qsip, resamples = 10, with_seed = 22),
                   run_resampling(test_qsip, resamples = 10, with_seed = 22))
})

test_that("resample number is correct", {
  expect_equal(length(run_resampling(test_qsip, resamples = 10, with_seed = 22)@resamples$u), 10)
  expect_equal(length(run_resampling(test_qsip, resamples = 10, with_seed = 22)@resamples$l), 10)})

