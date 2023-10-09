test_that("valid positions pass", {
  expect_null(validate_gradient_position(c(-1, 1, 3.000, 7, 11)))
})

test_that("positions given as strings fail", {
  expect_error(validate_gradient_position("1"))
  expect_error(validate_gradient_position(c(1, "1")))
})

test_that("positions as fractions fail", {
  expect_error(validate_gradient_position(c(-1, 1, 3.001, 7, 11)))
})
