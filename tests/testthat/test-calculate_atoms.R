test_that("Values for all three isotopes work", {
  expect_equal(calculate_atoms(.5, "13C"), 9.75)
  expect_equal(calculate_atoms(.5, "15N"), 3.75)
  expect_equal(calculate_atoms(.5, "18O"), 6)
})

test_that("Invalid isotopes give error", {
  expect_error(calculate_atoms(.5, "12C"))
  expect_error(calculate_atoms(.5, "14N"))
  expect_error(calculate_atoms(.5, "16O"))
})

test_that("GC given as string fails", {
  expect_error(calculate_atoms("0.5", "13C"))
})
