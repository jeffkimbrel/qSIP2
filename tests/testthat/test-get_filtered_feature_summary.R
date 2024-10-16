qsip_normal_strict_filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_filtered.rds"))


test_that("doesn't error", {
  expect_no_error(get_filtered_feature_summary(qsip_normal_strict_filtered, feature_id = "ASV_2"))
})


test_that("is the right size", {
  expect_equal(length(get_filtered_feature_summary(qsip_normal_strict_filtered, feature_id = "ASV_2")), 3)
})


test_that("unknown feature throws error", {
  expect_error(get_filtered_feature_summary(qsip_normal_strict_filtered, feature_id = "not_a_feature"), "not_a_feature is not a valid feature_id")
})
