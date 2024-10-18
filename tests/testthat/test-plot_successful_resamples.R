normal_qsip <- readRDS(test_path("fixtures", "qsip_normal_strict_EAF.rds"))

test_that("works as expected", {
  expect_true("ggplot" %in% class(plot_successful_resamples(normal_qsip)))
})


test_that("gives error if not qsip object or resampled", {
  expect_error(plot_successful_resamples(example_feature_df),
            "object must be a <qsip_data> object, not <tbl_df>")
  expect_error(plot_successful_resamples(example_qsip_object),
            "object is a non-resampled <qsip_data> object")
})

test_that("labels works with boolean, fails otherwise", {
  expect_no_error(plot_successful_resamples(normal_qsip, labels = TRUE))
  expect_error(plot_successful_resamples(normal_qsip, labels = "not_boolean"),
      "<labels> should be TRUE/FALSE")
})

test_that("as_percentage works with boolean, fails otherwise", {
  expect_no_error(plot_successful_resamples(normal_qsip, as_percentage = TRUE))
  expect_error(plot_successful_resamples(normal_qsip, as_percentage = "not_boolean"),
      "<as_percentage> should be TRUE/FALSE")
})
