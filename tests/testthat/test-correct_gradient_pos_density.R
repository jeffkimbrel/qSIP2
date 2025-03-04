test_that("returns sample data", {
  expect_true(inherits(correct_gradient_pos_density(example_sample_object), "qSIP2::qsip_sample_data"))
})

test_that("errors with wrong input", {
  expect_error(correct_gradient_pos_density(example_sample_df), "sample_data should be of class <qsip_sample_data>")
})


test_that("sensitivity values are OK", {
  expect_error(correct_gradient_pos_density(example_sample_object, sensitivity = -1), "sensitivity should be a positive number")
  expect_error(correct_gradient_pos_density(example_sample_object, sensitivity = "not a number"), "sensitivity should be a positive number")
  expect_no_error(correct_gradient_pos_density(example_sample_object, sensitivity = 1))
})


