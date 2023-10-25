test_that("Values are expected for 68% GC", {
  expect_equal(calculate_M_labeledmax(308.0283, 9.66, "13C"), 317.66373)
  expect_equal(calculate_M_labeledmax(308.0283, 3.84, "15N"), 311.88739)
  expect_equal(calculate_M_labeledmax(308.0283, 6, "18O"), 320.10577)
})

test_that("prop0 should only affect 18O 68% GC", {
  expect_equal(calculate_M_labeledmax(308.0283, 9.66, "13C"),
               calculate_M_labeledmax(308.0283, 9.66, "13C", propO = 0))

  expect_equal(calculate_M_labeledmax(308.0283, 3.84, "15N"),
               calculate_M_labeledmax(308.0283, 3.84, "15N", propO = 0))

  expect_false(calculate_M_labeledmax(308.0283, 6, "18O") == calculate_M_labeledmax(308.0283, 6, "18O", propO = .75))
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
