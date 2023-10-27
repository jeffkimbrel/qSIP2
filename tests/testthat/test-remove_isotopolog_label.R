test_that("conversion works", {
  testdf1 <- data.frame(
    "isotope" = rep("13C", 12),
    "isotopolog_label" = c(rep("natural abundance", 6), rep("isotopically labeled", 6))
  )
  expect_equal(remove_isotopolog_label(testdf1)$isotope, c(rep("12C", 6), rep("13C", 6)))
})

test_that("Missing columns trigger error", {
  testdf2 <- data.frame(
    "not_found" = rep("13C", 12),
    "isotopolog_label" = c(rep("natural abundance", 6), rep("isotopically labeled", 6))
  )
  expect_error(remove_isotopolog_label(testdf2),
               "ERROR: This dataframe doesn't appear to have isotope data")


  testdf3 <- data.frame(
    "isotope" = rep("13C", 12),
    "not_found" = c(rep("natural abundance", 6), rep("isotopically labeled", 6))
  )
  expect_error(remove_isotopolog_label(testdf3),
               "ERROR: This dataframe doesn't appear to have isotopolog_label data")
})

test_that("Unexpected isotopes produce error", {
  testdf4 <- data.frame(
    "isotope" = rep("13", 12),
    "isotopolog_label" = c(rep("natural abundance", 6), rep("isotopically labeled", 6))
  )
  expect_error(remove_isotopolog_label(testdf4), "ERROR: Please fix the isotope names and try again")
})
