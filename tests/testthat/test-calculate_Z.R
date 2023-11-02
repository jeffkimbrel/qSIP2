calculate_Z_testdf <- data.frame(
  labeled = c(1.6, 1.65, 1.75),
  unlabeled = c(1.6, 1.675, 1.8)
)

test_that("basic function works", {
  expect_equal(
    calculate_Z(calculate_Z_testdf$labeled, calculate_Z_testdf$unlabeled),
    c(0, -0.025, -0.05)
  )
})

test_that("unexpected WAD values fail", {
  expect_error(calculate_Z(c(NA, 1, 2), calculate_Z_testdf$unlabeled), "Can't calculate Z - some WAD values are <NA>")
  expect_error(calculate_Z(c("not_a_number", 1, 2), calculate_Z_testdf$unlabeled), "Can't calculate Z - some WAD values are not numeric")
  expect_error(calculate_Z(calculate_Z_testdf$labeled, c("not_a_number", 1, 2)), "Can't calculate Z - some WAD values are not numeric")
})
