test_that("Values are expected for 68% GC", {
  expect_equal(calculate_M_labeledmax(308.0283, 9.66, "13C"), 317.66373)
  expect_equal(calculate_M_labeledmax(308.0283, 3.84, "15N"), 311.88739)
  expect_equal(calculate_M_labeledmax(308.0283, 6, "18O"), 320.10577)
})

test_that("M is formatted correctly", {
  expect_error(calculate_M_labeledmax("310", 6, "18O"), class = "qsip_bad_MW_value")
})

test_that("isotope is formatted correctly", {
  expect_error(calculate_M_labeledmax(310, 6, "O"), "Please fix the isotope names and try again")
})