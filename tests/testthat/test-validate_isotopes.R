test_that("valid isotopes pass", {
  expect_null(validate_isotopes(isotope = c("12C")))
})

test_that("invalid isotopes fail and give message", {
  expect_error(validate_isotopes(isotope = c("12")))
  expect_error(validate_isotopes(isotope = 12))
})
