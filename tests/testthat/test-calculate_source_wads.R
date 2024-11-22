test_that("works correctly", {
  expect_snapshot(calculate_source_wads(example_sample_object))
})

test_that("wrong input type fails", {
  expect_error(calculate_source_wads(example_source_object),
               "sample_data should be of class <qsip_sample_data>")
})
