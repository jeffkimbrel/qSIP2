normal_qsip <- readRDS(test_path("fixtures", "qsip_normal_strict_EAF.rds"))
normal_qsip_failures <- readRDS(test_path("fixtures", "qsip_normal_failures_EAF.rds"))
multi_qsip <- readRDS(test_path("fixtures", "multi_qsip_EAF.rds"))

test_that("snapshots look as expected", {
  expect_snapshot(summarize_EAF_values(normal_qsip))
  expect_snapshot(summarize_EAF_values(normal_qsip, confidence = 0.95))
})

test_that("Wrong input types give error", {
  expect_error(
    summarize_EAF_values(example_feature_df),
    "ERROR: qsip_data_object must be of class <qsip_data> or <list> of qsip_data objects"
  )
  expect_error(
    summarize_EAF_values(test_qsip, confidence = "not_a_numeric"),
    "ERROR: confidence should be numeric"
  )
  expect_error(
    summarize_EAF_values(test_qsip, confidence = -2),
    "ERROR: confidence level should be between 0 and 1"
  )
  expect_error(
    summarize_EAF_values(test_qsip, confidence = 2),
    "ERROR: confidence level should be between 0 and 1"
  )
})

test_that("Make sure qsip object has EAF data", {
  expect_error(
    summarize_EAF_values(example_qsip_object),
    "object is a non-filtered <qsip_data> object"
  )
})


test_that("works on lists of qsip objects", {
  expect_snapshot(summarize_EAF_values(multi_qsip))
})

test_that("fails on list of non-qsip objects", {
  expect_error(summarize_EAF_values(list("A" = 123)), "qsip_data_object must be of class <qsip_data> or <list> of qsip_data objects")
})
