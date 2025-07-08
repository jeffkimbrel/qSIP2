test_that("works as expected, or at least makes a ggplot", {
  expect_true("ggplot" %in% class(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969")))
})

test_that("wrong type gives error", {
  expect_error(plot_feature_occurrence(isotope_palette),
               "qsip_data_object should be class <qsip_data>")
})

test_that("incorrect feature_ids gives error", {
  expect_error(plot_feature_occurrence(example_qsip_object, feature_ids = example_feature_df),
               "<feature_ids> argument must be NULL or a vector of strings")
})

test_that("given scale argument OK", {
  expect_no_error(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969", scale = "source"))
  expect_no_error(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969", scale = "feature"))
  expect_error(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969", scale = "not_valid"),
               "scale must be 'none', 'feature', or 'source'")
})

test_that("show_wad should be a boolean", {
  expect_no_error(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969", show_wad = TRUE))
  expect_no_error(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969", show_wad = FALSE))
  expect_error(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969", show_wad = "YES"),
               "show_wad must be a boolean")
})

test_that("title should be a character of length one", {
  expect_no_error(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969", title = "A good title"))
  expect_error(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969", title = c("title 1", "why a second title?")),
               "title should only have a length one 1")
  expect_error(plot_feature_occurrence(example_qsip_object, feature_ids = "ASV_969", title = example_feature_df),
               "title must be a character")
})
