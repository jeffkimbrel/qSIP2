test_that("works correctly", {
  expect_equal(
    calculate_M(.5),
    307.939
  )
  expect_equal(
    calculate_M(0),
    307.691
  )
  expect_equal(
    calculate_M(1),
    308.187
  )
})


test_that("Passing a string produces an error", {
  expect_error(calculate_M(".5"))
})
