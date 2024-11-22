normal_qsip <- readRDS(test_path("fixtures", "qsip_normal_strict_EAF.rds"))
normal_qsip_failures <- readRDS(test_path("fixtures", "qsip_normal_failures_EAF.rds"))
multi_qsip <- readRDS(test_path("fixtures", "multi_qsip_EAF.rds"))

test_that("fails if wrong object given", {
  expect_error(plot_EAF_values(example_feature_df), "qsip_data_object must be of class <qsip_data> or <list> of qsip_data objects")
})

test_that("plot and layers look as expected", {
  p <- plot_EAF_values(normal_qsip)
  expect_equal(p$labels$x, "observed_EAF")

  p <- plot_EAF_values(multi_qsip)
  expect_equal(p$labels$x, "observed_EAF")
})


test_that("plot and layers look as expected when allow failures = t", {
  p <- plot_EAF_values(normal_qsip_failures)
  expect_equal(p$labels$x, "observed_EAF")
})

test_that("changing confidence changes message", {
  expect_message(plot_EAF_values(multi_qsip, confidence = 0.90), "Confidence level = 0.9")
  expect_message(plot_EAF_values(multi_qsip, confidence = 0.95), "Confidence level = 0.95")
})

test_that("top n works", {
  p5 <- plot_EAF_values(normal_qsip, top = 5)
  expect_equal(length(p5$data$observed_EAF), 5)

  p10 <- plot_EAF_values(normal_qsip, top = 10)
  expect_equal(length(p10$data$observed_EAF), 10)
})

test_that("confidence interval within range", {
  expect_error(plot_EAF_values(normal_qsip, confidence = 1.1), "confidence level should be between 0 and 1")
  expect_error(plot_EAF_values(normal_qsip, confidence = 0), "confidence level should be between 0 and 1")
})

test_that("success_ratio within range", {
  expect_error(plot_EAF_values(normal_qsip, success_ratio = 1.1), "success_ratio should be between 0 and 1")
  expect_error(plot_EAF_values(normal_qsip, success_ratio = 0), "success_ratio should be between 0 and 1")
})

test_that("alpha within range", {
  expect_error(plot_EAF_values(normal_qsip, alpha = 1.1), "alpha level should be between 0 and 1")
  expect_error(plot_EAF_values(normal_qsip, alpha = 0), "alpha level should be between 0 and 1")
})

test_that("error type is correct", {
  expect_error(plot_EAF_values(normal_qsip, error = "foo"), "<error> should be 'none', 'bar' or 'ribbon', not foo")
  expect_no_error(plot_EAF_values(normal_qsip, error = "bar"))
  expect_no_error(plot_EAF_values(normal_qsip, error = "ribbon"))
  expect_no_error(plot_EAF_values(normal_qsip, error = "none"))
})


test_that("titling works", {
  p <- plot_EAF_values(normal_qsip)
  expect_null(p$labels$title)

  p <- plot_EAF_values(normal_qsip, title = "My Title")
  expect_equal(p$labels$title, "My Title")
})
