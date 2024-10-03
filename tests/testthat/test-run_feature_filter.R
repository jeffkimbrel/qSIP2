test_that("works properly", {
  expect_snapshot(run_feature_filter(example_qsip_object,
    unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
    labeled_source_mat_ids = c("S178", "S179", "S180")
  ))
})

test_that("fails if non-qsip object provided", {
  expect_error(run_feature_filter(example_sample_df),
    "<qsip_data_object> must be of class qsip_data"
  )
})

test_that("minimums larger than possible give error", {
  expect_error(
    run_feature_filter(example_qsip_object,
      unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
      labeled_source_mat_ids = c("S178", "S179", "S180"),
      min_unlabeled_sources = 10
    ),
    "min_unlabeled_sources is set to 10 but unlabeled_source_mat_ids only has 8"
  )
  expect_error(
    run_feature_filter(example_qsip_object,
      unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
      labeled_source_mat_ids = c("S178", "S179", "S180"),
      min_labeled_sources = 5
    ),
    "min_labeled_sources is set to 5 but labeled_source_mat_ids only has 3"
  )
})

test_that("source_mat_ids with isotopes inconsistent with label designation fail", {
  expect_error(
    run_feature_filter(example_qsip_object,
      unlabeled_source_mat_ids = c("S178", "S179", "S180"),
      labeled_source_mat_ids = c("S178", "S179", "S180")
    ),
    "some of the unlabeled_source_mat_ids have a heavy isotope designation"
  )
  expect_error(
    run_feature_filter(example_qsip_object,
                       unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
                       labeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C")
    ),
    "some of the labeled_source_mat_ids have a light isotope designation"
  )
})


test_that("source_mat_ids not found fail", {
  expect_error(
    run_feature_filter(example_qsip_object,
                       unlabeled_source_mat_ids = c("S149", "S150", "not_found"),
                       labeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "13C")
    ),
    "Some given unlabeled_source_mat_ids are not found"
  )
  expect_error(
    run_feature_filter(example_qsip_object,
                       unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
                       labeled_source_mat_ids = c("S178", "S179", "not_found")
    ),
    "Some given labeled_source_mat_ids are not found"
  )
})
