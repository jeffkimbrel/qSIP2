test_that("calculations work", {
  expect_equal(
    calculate_EAF(M_labeled = 308.2527, M = 308.0259, M_labeledmax = 317.6637, isotope = "13C"),
    0.02327084, tolerance = 8
  )

  expect_type(calculate_EAF(M_labeled = 308.2527, M = 308.0259, M_labeledmax = 317.6637, isotope = "13C"), "double")
})


test_that("M* values that are not numeric give error", {
  expect_error(
    calculate_EAF(M_labeled = "not_a_number", M = 308.0259, M_labeledmax = 317.6637, isotope = "13C"),
    "M_labeled should be class <numeric>, not character"
  )
  expect_error(
    calculate_EAF(M_labeled = 308.2527, M = "not_a_number", M_labeledmax = 317.6637, isotope = "13C"),
    "M should be class <numeric>, not character"
  )
  expect_error(
    calculate_EAF(M_labeled = 308.2527, M = 308.0259, M_labeledmax = "not_a_number", isotope = "13C"),
    "M_labeledmax should be class <numeric>, not character"
  )
})
