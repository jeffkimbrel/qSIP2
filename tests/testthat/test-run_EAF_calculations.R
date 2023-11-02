test_qsip = example_qsip_object |>
  run_feature_filter(  unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
                       labeled_source_mat_ids = c("S178", "S179", "S180"),
                       min_unlabeled_sources = 6,
                       min_labeled_sources = 3,
                       min_unlabeled_fractions = 6,
                       min_labeled_fractions = 6) |>
  run_resampling(resamples = 100, with_seed = 44)



test_that("works correctly", {
  expect_snapshot(run_EAF_calculations(test_qsip))
})

test_that("wrong type or not run through pre-steps", {
  expect_error(run_EAF_calculations(example_feature_object),
               "qsip_data_object should be class <qsip_data>")
  expect_error(run_EAF_calculations(example_qsip_object))
})
