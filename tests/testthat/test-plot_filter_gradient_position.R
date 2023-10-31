qsip_filtered_object <- run_feature_filter(example_qsip_object,
  unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
  labeled_source_mat_ids = c("S178", "S179", "S180"),
  min_unlabeled_sources = 3,
  min_labeled_sources = 3,
  min_unlabeled_fractions = 6,
  min_labeled_fractions = 6
)


test_that("works as expected", {
  expect_true("ggplot" %in% class(plot_filter_gradient_position(qsip_filtered_object)))
  expect_true("ggplot" %in% class(plot_filter_gradient_position(qsip_filtered_object,
    colors = c("red", "blue", "black")
  )))

  expect_true("ggplot" %in% class(plot_filter_gradient_position(qsip_filtered_object,
    return_type = "combined"
  )))
  expect_type(plot_filter_gradient_position(qsip_filtered_object,
    return_type = "individual"
  ), "list")
  expect_type(plot_filter_gradient_position(qsip_filtered_object,
    return_type = "dataframe"
  ), "list")
})

test_that("incorrect return type fails", {
  expect_error(plot_filter_gradient_position(qsip_filtered_object, return_type = "not_valid"))
})

test_that("running on object that hasn't been filtered fails", {
  expect_error(plot_filter_gradient_position(example_qsip_object))
})

test_that("giving the wrong object type fails", {
  expect_error(plot_filter_gradient_position(example_feature_object))
})
