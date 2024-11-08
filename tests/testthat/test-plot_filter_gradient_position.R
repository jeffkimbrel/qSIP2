qsip_normal_strict_filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_filtered.rds"))

test_that("works as expected", {
  expect_true("ggplot" %in% class(plot_filter_results(qsip_normal_strict_filtered)))
  expect_true("ggplot" %in% class(plot_filter_results(qsip_normal_strict_filtered,
    colors = c("red", "blue", "black")
  )))

  expect_true("ggplot" %in% class(plot_filter_results(qsip_normal_strict_filtered,
    return_type = "combined"
  )))
  expect_type(plot_filter_results(qsip_normal_strict_filtered,
    return_type = "individual"
  ), "list")
  expect_type(plot_filter_results(qsip_normal_strict_filtered,
    return_type = "dataframe"
  ), "list")
})

test_that("incorrect return type fails", {
  expect_error(plot_filter_results(qsip_normal_strict_filtered, return_type = "not_valid"))
})


test_that("running on object that hasn't been filtered fails", {
  expect_error(plot_filter_results(example_qsip_object))
})

test_that("giving the wrong object type fails", {
  expect_error(plot_filter_results(example_feature_object))
})
