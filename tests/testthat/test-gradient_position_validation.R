test_that("valid positions pass", {
  expect_null(gradient_position_validation(c(-1, 1, 3.000, 7, 11)))
})

test_that("positions given as strings fail", {
  expect_error(gradient_position_validation("1"))
  expect_error(gradient_position_validation(c(1, "1")))
})

test_that("positions as fractions fail", {
  expect_error(gradient_position_validation(c(-1, 1, 3.001, 7, 11)))
})
