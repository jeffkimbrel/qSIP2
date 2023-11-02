test_that("wrong input types fail", {
  expect_error(plot_density_outliers(example_feature_object),
               "sample_data should be class <qsip_sample_data> or <qsip_data>, not qsip_feature_data")
  expect_error(plot_density_outliers(example_qsip_object, sensitivity = "not_numeric"),
               "sensitivity should be a <numeric>")
})
