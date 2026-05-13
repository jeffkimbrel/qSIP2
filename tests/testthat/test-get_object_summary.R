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

test_that("uses group values in group column", {
  li <- list("A" = qsip_normal_strict_filtered, "B" = qsip_normal_strict_filtered)
  suppressWarnings({
    result <- get_object_summary(li)
  })
  expect_true("group" %in% names(result))
  expect_true(nrow(result) == 2)
})

test_that("warns when duplicate group names are detected", {
  li <- list("A" = example_qsip_object, "B" = example_qsip_object)
  expect_warning(
    get_object_summary(li),
    "Duplicate group names detected"
  )
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

test_that("works with source_format = 'ids' for single qsip_data object", {
  result <- get_object_summary(qsip_normal_strict_filtered, source_format = "ids")
  expect_snapshot(result)
  expect_true("unlabeled_source_ids" %in% names(result))
  expect_true("labeled_source_ids" %in% names(result))
  expect_true(is.list(result$unlabeled_source_ids))
  expect_true(is.list(result$labeled_source_ids))
})

test_that("works with source_format = 'ids' for list of qsip_data objects", {
  li <- list("A" = qsip_normal_strict_filtered, "B" = qsip_normal_strict_filtered)
  suppressWarnings({
    result <- get_object_summary(li, source_format = "ids")
  })
  expect_snapshot(result)
  expect_true("unlabeled_source_ids" %in% names(result))
  expect_true("labeled_source_ids" %in% names(result))
})

test_that("source_format = 'count' returns count columns (default)", {
  result <- get_object_summary(qsip_normal_strict_filtered, source_format = "count")
  expect_true("unlabeled_source_count" %in% names(result))
  expect_true("labeled_source_count" %in% names(result))
  expect_false("unlabeled_source_ids" %in% names(result))
  expect_false("labeled_source_ids" %in% names(result))
})
