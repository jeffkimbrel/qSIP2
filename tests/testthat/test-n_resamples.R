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

test_that("qsip_data object works", {
  expect_equal(n_resamples(test_qsip), 1000)
})

test_that("list of qsip_data object works", {
  expect_snapshot(n_resamples(list("A" = example_qsip_object,
                               "B" = test_qsip)))
})


test_that("wrong object type gives error", {
  expect_error(n_resamples(example_feature_df), "this function requires a <qsip_data> object, or a list of <qsip_data> objects")
})
