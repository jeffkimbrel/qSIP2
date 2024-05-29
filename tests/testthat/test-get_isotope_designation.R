test_qsip <- example_qsip_object |>
  run_feature_filter(
    unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
    labeled_source_mat_ids = c("S178", "S179", "S180"),
    min_unlabeled_sources = 6,
    min_labeled_sources = 3,
    min_unlabeled_fractions = 6,
    min_labeled_fractions = 6
  )

test_that("Passing wrong object gives error", {
  expect_error(get_isotope_designation(example_feature_object),
               "qsip_data_object must be of class <qsip_data>")
})

test_that("Returns the right value", {
  expect_equal(get_isotope_designation(test_qsip), "13C")
})
