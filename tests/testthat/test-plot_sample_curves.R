test_that("works as expected, or at least makes a ggplot", {
  expect_true("ggplot" %in% class(plot_sample_curves(example_qsip_object)))
})

test_that("title works correctly", {
  expect_equal("test", plot_sample_curves(example_qsip_object, title = "test")$labels$title)
  expect_error(plot_sample_curves(example_qsip_object, title = example_feature_df), "title must be a character string")
})

test_that("data with unfractionated samples produce message", {
  expect_message(plot_sample_curves(example_qsip_growth_object), "some unfractionated samples have been filtered from this plot")
})
