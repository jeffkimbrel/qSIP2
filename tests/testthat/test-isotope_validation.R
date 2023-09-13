test_that("valid isotopes pass", {
  expect_true(isotope_validation(isotope = c("12C")))
})

test_that("invalid isotopes fail and give message", {
  expect_message(isotope_validation(isotope = c("12")))
  expect_message(isotope_validation(isotope = 12))
})
