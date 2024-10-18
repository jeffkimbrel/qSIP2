test_that("works with qsip data or sample data", {
  expect_no_error(plot_density_outliers(example_qsip_object))
  expect_no_error(plot_density_outliers(example_sample_object))
})

test_that("wrong input types fail", {
  expect_error(plot_density_outliers(example_feature_object),
               "sample_data should be class <qsip_sample_data> or <qsip_data>, not qsip_feature_data")
  expect_error(plot_density_outliers(example_qsip_object, sensitivity = "not_numeric"),
               "sensitivity should be a <numeric>")
})

test_that("warns about unfractionated data", {
  expect_message(plot_density_outliers(example_qsip_growth_object), "some unfractionated samples have been filtered from this plot")
})

# test sensitivity is numeric
test_that("sensitivity is numeric", {
  expect_error(plot_density_outliers(example_qsip_object, sensitivity = "not_numeric"),
               "sensitivity should be a <numeric>")
})
