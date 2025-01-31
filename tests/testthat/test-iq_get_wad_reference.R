test_that("works as expected", {
  expect_snapshot(iq_get_wad_reference(example_qsip_object))
})

test_that("non-qsip errors caught", {
  expect_error(iq_get_wad_reference(example_sample_df),
               "<qsip_data_object> must be of class qsip_data")
})

test_that("fraction_cutoff errors caught", {
  expect_error(iq_get_wad_reference(example_qsip_object, fraction_cutoff = 0),
               "<fraction_cutoff> must be an integer greater than 0")
})

test_that("source_cutoff errors caught", {
  expect_error(iq_get_wad_reference(example_qsip_object, source_cutoff = 0),
               "<source_cutoff> must be an integer greater than 0")
})

