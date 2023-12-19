test_that("valid isotopes pass", {
  expect_null(validate_isotopes(isotope = c("12C")))
})

test_that("invalid isotopes fail and give message", {
  expect_error(validate_isotopes(isotope = c("12")))
  expect_error(validate_isotopes(isotope = 12))
})

test_that("Typical unfractionated terms give message but don't fail", {
  expect_message(validate_isotopes(isotope = c("16O", "bulk")), "Isotope value found that matches typical unfractionated terms: bulk")
  expect_message(validate_isotopes(isotope = c("bulk")), "Isotope value found that matches typical unfractionated terms: bulk")
})
