test_that("Values are expected", {
  expect_equal(calculate_M_labeledmax(310, 6, "18O", propO = 0), 310)
})

test_that("M is formatted correctly", {
  expect_error(calculate_M_labeledmax("310", 6, "18O"))
})

test_that("isotope is formatted correctly", {
  expect_error(calculate_M_labeledmax(310, 6, "O"))
})

test_that("propO is formatted correctly", {
  expect_error(calculate_M_labeledmax(310, 6, "18O", propO = -1))
  expect_error(calculate_M_labeledmax(310, 6, "18O", propO = 2))
})
