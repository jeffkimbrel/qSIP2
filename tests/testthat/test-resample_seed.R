qsip_normal_strict_resampled <- readRDS(test_path("fixtures", "qsip_normal_strict_resampled.rds"))
qsip_normal_strict_filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_filtered.rds"))

test_that("works correctly", {
  expect_equal(resample_seed(qsip_normal_strict_resampled), 43)
})

test_that("object not resampled returns NULL", {
  expect_null(resample_seed(qsip_normal_strict_filtered))
})

test_that("list object gives named dataframe", {
  expect_equal(
    resample_seed(list(qsip_normal_strict_resampled)),
    structure(list(group = 1L, seed = 43),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -1L)
    )
  )
})

test_that("non qsip object gives error", {
  expect_error(resample_seed(example_feature_df),
               "this function requires a <qsip_data> object, or a list of <qsip_data> objects")
})
