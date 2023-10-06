test_that("valid isotopes pass", {
  expect_null(isotope_validation(isotope = c("12C")))
})

test_that("invalid isotopes fail and give message", {
  expect_error(isotope_validation(isotope = c("12")))
  expect_error(isotope_validation(isotope = 12))
})
