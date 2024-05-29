test_that("Passing wrong object gives error", {
  expect_error(get_isotope_designation(example_feature_object, c("S149","S150","S151"), c("S178", "S179", "S180")),
               "qsip_data_object must be a <qsip_data> object, not <qsip_feature_data>")
})

test_that("Returns the right value", {
  expect_equal(get_isotope_designation(example_qsip_object, c("S149","S150","S151"), c("S178", "S179", "S180")), "13C")
})

test_that("improper unlabeled source_mat_ids fail", {
  expect_error(get_isotope_designation(example_qsip_object, 1, c("S178", "S179", "S180")),
               "unlabeled_source_mat_ids must be a character vector with a size of 1 or more")
})

test_that("improper labeled source_mat_ids fail", {
  expect_error(get_isotope_designation(example_qsip_object, c("S149","S150","S151"), 123),
               "labeled_source_mat_ids must be a character vector with a size of 1 or more")
})

test_that("heavy isotope given as unlabeled gives error", {
  expect_error(get_isotope_designation(example_qsip_object, c("S178", "S179", "S180"), c("S149","S150","S151")),
               "Please fix the isotope names and try again")
})
