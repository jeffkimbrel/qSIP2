Z_matrix <- readRDS(test_path("fixtures", "Z_matrix.rds"))

test_that("basic function works", {
  expect_equal(
    calculate_Z(Z_matrix$labeled, Z_matrix$unlabeled),
    c(0, -0.025, -0.05)
  )
})

test_that("unexpected WAD values fail", {
  expect_error(calculate_Z(c(NA, 1, 2), Z_matrix$unlabeled), "Can't calculate Z - some WAD values are <NA>")
  expect_error(calculate_Z(c("not_a_number", 1, 2), Z_matrix$unlabeled), "Can't calculate Z - some WAD values are not numeric")
  expect_error(calculate_Z(Z_matrix$labeled, c("not_a_number", 1, 2)), "Can't calculate Z - some WAD values are not numeric")
})
