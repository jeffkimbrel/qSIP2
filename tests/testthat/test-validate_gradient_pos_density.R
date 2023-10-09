test_that("valid densities pass", {
  expect_null(validate_gradient_pos_density(c(1.6, 1.7, 1.8), high = 1.8, low = 1.6))
})

test_that("densities given as strings fail", {
  expect_error(validate_gradient_pos_density("1.7"))
  expect_error(validate_gradient_pos_density(c(1.6, "1.7")))
})

test_that("densities outside of the valid range fail", {
  expect_error(validate_gradient_pos_density(c(1.6, 1.7, 1.8), high = 1.79))
  expect_error(validate_gradient_pos_density(c(1.6, 1.7, 1.8), low = 1.61))
})
