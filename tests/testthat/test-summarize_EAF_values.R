# make complete qSIP object
test_qsip <- qsip_data(example_source_object, example_sample_object, example_feature_object) |>
  run_feature_filter(
    unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
    labeled_source_mat_ids = c("S178", "S179", "S180"),
    min_unlabeled_sources = 3,
    min_labeled_sources = 3,
    min_unlabeled_fractions = 6,
    min_labeled_fractions = 6
  ) |>
  run_resampling(resamples = 1000, with_seed = 43) |>
  run_EAF_calculations()

test_that("snapshots look as expected", {
  expect_snapshot(summarize_EAF_values(test_qsip))
  expect_snapshot(summarize_EAF_values(test_qsip, confidence = 0.95))
})

test_that("Wrong input types give error", {
  expect_error(
    summarize_EAF_values(example_feature_df),
    "ERROR: qsip_data_object must be of class <qsip_data> or <list> of qsip_data objects"
  )
  expect_error(
    summarize_EAF_values(test_qsip, confidence = "not_a_numeric"),
    "ERROR: confidence should be numeric"
  )
  expect_error(
    summarize_EAF_values(test_qsip, confidence = -2),
    "ERROR: confidence level should be between 0 and 1"
  )
  expect_error(
    summarize_EAF_values(test_qsip, confidence = 2),
    "ERROR: confidence level should be between 0 and 1"
  )
})

test_that("Make sure qsip object has EAF data", {
  expect_error(
    summarize_EAF_values(example_qsip_object),
    "ERROR: @EAF slot is empty, have you run run_EAF_calculations()?"
  )
})

