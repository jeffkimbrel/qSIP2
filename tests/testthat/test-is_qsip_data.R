test_that("correct type gives NULL", {
  expect_true(is_qsip_data(example_qsip_object))
})

test_that("wrong type gives an error", {
  expect_error(is_qsip_data(1, error = TRUE), "object must be a <qsip_data> object, not <numeric>")
})

test_that("wrong type gives an FALSE when error = FALSE", {
  expect_false(is_qsip_data(1, error = FALSE))
})

test_that("other qsip types also still give an error", {
  expect_false(is_qsip_data(example_feature_object))
})

test_that("give error parameter is not logical", {
  expect_error(is_qsip_data(example_qsip_object, error = 1), "error must be a logical")
})
