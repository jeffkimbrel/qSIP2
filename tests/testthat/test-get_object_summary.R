qsip_normal_strict_filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_filtered.rds"))

test_that("works with single qsip_source_data object", {
  expect_snapshot(get_object_summary(example_source_object))
})

test_that("works with single qsip_sample_data object", {
  expect_snapshot(get_object_summary(example_sample_object))
})

test_that("works with single qsip_feature_data object", {
  expect_snapshot(get_object_summary(example_feature_object))
})

test_that("works with single qsip_data object", {
  expect_snapshot(get_object_summary(example_qsip_object))
})

test_that("works with single qsip_data object with a group", {
  expect_snapshot(get_object_summary(qsip_normal_strict_filtered))
})

test_that("works with a named list of qsip_data objects", {
  li <- list("A" = example_qsip_object, "B" = example_qsip_object)
  expect_snapshot(get_object_summary(li))
})

test_that("uses group values as column names in list", {
  li <- list("A" = qsip_normal_strict_filtered, "B" = qsip_normal_strict_filtered)
  result <- get_object_summary(li)
  expect_true(any(names(result) != c("metric", "A", "B")))
})

test_that("deduplicates column names with _1 suffix", {
  li <- list("A" = example_qsip_object, "B" = example_qsip_object)
  result <- get_object_summary(li)
  expect_true(ncol(result) == 3)
})

test_that("errors on unnamed list", {
  li <- list(example_qsip_object, example_qsip_object)
  expect_error(get_object_summary(li))
})

test_that("errors on mixed S7 types in list", {
  li <- list("A" = example_source_object, "B" = example_sample_object)
  expect_error(get_object_summary(li))
})

test_that("errors on non-qsip input", {
  expect_error(get_object_summary(example_source_df))
})
