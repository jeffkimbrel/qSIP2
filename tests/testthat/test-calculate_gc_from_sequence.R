test_that("works OK", {
  expect_equal(calculate_gc_from_sequence("AAA"), 0)
  expect_equal(calculate_gc_from_sequence("GGG"), 1)
  expect_equal(calculate_gc_from_sequence("AAGG"), 0.5)
  expect_equal(calculate_gc_from_sequence("AaGg"), 0.5)
})

test_that("errors with invalid input", {
  expect_error(calculate_gc_from_sequence("CGCTA "), "<sequence> appears to contain invalid characters \\(not ACGT/acgt\\)")
  expect_error(calculate_gc_from_sequence("123"), "<sequence> appears to contain invalid characters \\(not ACGT/acgt\\)")
  expect_error(calculate_gc_from_sequence(example_feature_df), "Please provide a string sequence to calculate GC%")
})


test_that("multiple values work", {
  expect_equal(
    calculate_gc_from_sequence(c("GGCAG", "CAAGC")),
    c(0.8, 0.6)
  )
  expect_error(
    calculate_gc_from_sequence(c("GGCAG", 123)), "<sequence> appears to contain invalid characters \\(not ACGT/acgt\\)"
  )
})
