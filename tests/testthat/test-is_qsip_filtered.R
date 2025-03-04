qsip_normal_strict_filtered <- readRDS(test_path("fixtures", "qsip_normal_strict_filtered.rds"))

test_that("returns TRUE/FALSE", {
  expect_true(is_qsip_filtered(qsip_normal_strict_filtered))
  expect_false(is_qsip_filtered(example_qsip_object))
})

test_that("returns TRUE/FALSE", {
  expect_true(is_qsip_filtered(qsip_normal_strict_filtered, error = T))
  expect_error(is_qsip_filtered(example_qsip_object, error = T), "object is a non-filtered <qsip_data> object")
})
