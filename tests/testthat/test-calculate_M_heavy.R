test_that("works OK", {
  expect_equal(calculate_M_heavy(0, 300), 300)
  expect_equal(calculate_M_heavy(1, 300), 300 + 12.07747)
})


test_that("handles incorrect input", {
  expect_error(calculate_M_heavy(-1, 300), "prop0 should be between 0 and 1")
  expect_error(calculate_M_heavy(2, 300), "prop0 should be between 0 and 1")
  expect_error(calculate_M_heavy("not a number", 300), "prop0 should be between 0 and 1")
  expect_error(calculate_M_heavy(0.5, "not a number"), "M should be class <numeric>")
})
