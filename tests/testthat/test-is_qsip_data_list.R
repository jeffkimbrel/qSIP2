test_that("returns TRUE when it is a list of qsip objects", {
  expect_true(is_qsip_data_list(list("A" = example_qsip_object)))
})


test_that("naked qsip object gives error when set to return an error", {
  expect_error(is_qsip_data_list(example_qsip_object, error = TRUE), "object must be a list")
})

test_that("naked qsip object gives FALSE when set to return a logical", {
  expect_false(is_qsip_data_list(example_qsip_object, error = FALSE))
})


test_that("non-list gives error when set to return an error", {
  expect_error(is_qsip_data_list(123, error = TRUE), "object must be a list")
})

test_that("non-list gives FALSE when set to return a logical", {
  expect_false(is_qsip_data_list(123, error = FALSE))
})


test_that("list with non-qsip objects gives error when set to return an error", {
  expect_error(is_qsip_data_list(list("A" = 123, "B" = example_qsip_object), error = TRUE))
})

test_that("list with non-qsip objects gives FALSE when set to return a logical", {
  expect_false(is_qsip_data_list(list("A" = 123, "B" = example_qsip_object), error = FALSE))
})
