qsip_normal_strict_resampled <- readRDS(test_path("fixtures", "qsip_normal_strict_resampled.rds"))
qsip_drought_strict_resampled <- readRDS(test_path("fixtures", "qsip_drought_strict_resampled.rds"))

test_that("qsip_data object works", {
  expect_equal(n_resamples(qsip_normal_strict_resampled), 1000)
})

test_that("list of qsip_data object works", {
  expect_snapshot(n_resamples(list("A" = qsip_drought_strict_resampled,
                                   "B" = qsip_normal_strict_resampled)))
})

test_that("wrong object type gives error", {
  expect_error(n_resamples(example_feature_df), "this function requires a <qsip_data> object, or a list of <qsip_data> objects")
})
