qsip_normal_strict_resampled <- readRDS(test_path("fixtures", "qsip_normal_strict_resampled.rds"))
qsip_normal_strict_filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_filtered.rds"))

test_that("works correctly", {
  expect_equal(resample_seed(qsip_normal_strict_resampled), 43)
})

test_that("object not resampled returns NULL", {
  expect_null(resample_seed(qsip_normal_strict_filtered))
})
