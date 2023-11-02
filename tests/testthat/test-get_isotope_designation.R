test_that("Passing wrong object gives error", {
  expect_error(get_isotope_designation(example_feature_object),
               "qsip_data_object must be of class <qsip_data>")
})

test_that("Returns the right value", {
  expect_equal(get_isotope_designation(example_qsip_object), "13C")
})
