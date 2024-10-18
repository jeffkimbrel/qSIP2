qsip_normal_strict_filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_filtered.rds"))

test_that("works as expected", {
  expect_no_error(get_source_mat_ids(example_qsip_object))
  expect_equal(length(get_source_mat_ids(example_qsip_object)), 15)

  expect_no_error(get_source_mat_ids(qsip_normal_strict_filtered, filtered = TRUE))
  expect_equal(length(get_source_mat_ids(qsip_normal_strict_filtered, filtered = TRUE)), 11)
  expect_equal(length(get_source_mat_ids(qsip_normal_strict_filtered, filtered = FALSE)), 15)
})

test_that("non-qsip object object fails", {
  expect_error(get_source_mat_ids(example_feature_df),
               "object must be a <qsip_data> object, not <tbl_df>")
})

test_that("return filtered with a non-filtered object fails", {
  expect_error(get_source_mat_ids(example_qsip_object, filtered = TRUE),
               "No filtered source_mat_ids in this <qsip_data> object. Try filtering first, or setting the <filtered> to FALSE")
})

test_that("giving non-boolean to filtered gives error", {
  expect_error(get_source_mat_ids(example_qsip_object, filtered = "not_a_boolean"),
               "<filtered> must be TRUE/FALSE")
})
