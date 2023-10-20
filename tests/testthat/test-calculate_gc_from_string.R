test_that("works as intended", {
  expect_equal(calculate_gc_from_sequence("GGGGCCAA"), 0.75)
})

test_that("multiple values work", {
  expect_equal(
    calculate_gc_from_sequence(c("GGCAG", "CAAGC")),
    c(0.8, 0.6)
  )
})

test_that("non-strings fail", {
  expect_error(calculate_gc_from_sequence(1.7))
})
