test_that("Works as expected", {
  expect_equal(calculate_M_labeled(308, 1.7, 1.6), 327.25)
  expect_gt(calculate_M_labeled(308, 1.7, 1.6), 308)
  expect_lt(calculate_M_labeled(308, 1.6, 1.7), 308)
})

test_that("Non-numeric values produce error", {
  expect_error(calculate_M_labeled("308", 1.7, 1.6), "some input values not class <numeric>")
  expect_error(calculate_M_labeled(308, "1.7", 1.6), "some input values not class <numeric>")
  expect_error(calculate_M_labeled(308, 1.7, "1.6"), "some input values not class <numeric>")
})
