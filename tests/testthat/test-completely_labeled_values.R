test_that("Return values are expected", {
  expect_equal(completely_labeled_values("13C"), 0.98888767)
  expect_equal(completely_labeled_values("15N"), 0.996337)
  expect_equal(completely_labeled_values("18O"), 0.99799957)
})

test_that("Unknown isotopes give error", {
  expect_error(completely_labeled_values("not_an_isotope"), "Please fix the isotope names and try again")
})
