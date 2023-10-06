test_that("works as intended", {
  expect_equal(calculate_gc_from_density(1.68), 0.40646838)
})

test_that("multiple values work", {
  expect_equal(calculate_gc_from_density(c(1.6, 1.65)),
               c(-0.55154666614784, 0.0472127371713998))
})

test_that("densities given as strings fail", {
  expect_error(calculate_gc_from_density("1.7"))
})

test_that("different methods work", {
  expect_equal(calculate_gc_from_density(1.7, method = "MM"), 0.64597214)
  expect_equal(calculate_gc_from_density(1.7, method = "S"), 0.40816327)
})

test_that("wrong method fails", {
  expect_error(calculate_gc_from_density(1.66, method = "no_method_exists"))
})

test_that("default is MM", {
  expect_equal(calculate_gc_from_density(1.7, method = "MM"),
               calculate_gc_from_density(1.7))
})
