filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_EAF.rds"))


test_that("works as expected, or at least makes a ggplot", {
  expect_true("ggplot" %in% class(plot_feature_resamplings(filtered)))
})

test_that("wrong type or not run through pre-steps", {
  expect_error(plot_feature_resamplings(isotope_palette),
               "object must be a <qsip_data> object, not <character>")
  expect_error(plot_feature_resamplings(example_qsip_object),
               "This function requires a qsip object that has been run through run_resampling()")
})

test_that("wrong type of feature_ids not accepted", {
  expect_error(plot_feature_resamplings(filtered, feature_ids = example_feature_df),
               "<feature_ids> argument must be NULL or a vector of strings")
  expect_error(plot_feature_resamplings(filtered, feature_ids = "not_found"),
               "None of the features in feature_ids were found in the <qsip_data_object>")
  expect_message(plot_feature_resamplings(filtered, feature_ids = c("not_found", "ASV_104")),
               "Some of the features in feature_ids were not found in the <qsip_data_object>")
})

test_that("area expects boolean", {
  expect_no_error(plot_feature_resamplings(filtered, area = TRUE))
  expect_no_error(plot_feature_resamplings(filtered, area = FALSE))
  expect_error(plot_feature_resamplings(filtered, area = "YES"))
})

test_that("selected intervals work", {
  expect_no_error(plot_feature_resamplings(filtered, interval = "bar"))
  expect_no_error(plot_feature_resamplings(filtered, interval = "line"))
  expect_error(plot_feature_resamplings(filtered, interval = "ribbon"), "<intervals> argument must be 'bar' or 'line'")
})

test_that("confidence interval within range", {
  expect_error(plot_feature_resamplings(filtered, confidence = 1.1), "<confidence> argument must be between 0 and 1")
  expect_error(plot_feature_resamplings(filtered, confidence = 0), "<confidence> argument must be between 0 and 1")
})
