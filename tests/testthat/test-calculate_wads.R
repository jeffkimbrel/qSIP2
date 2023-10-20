test_that("dimensions of returned data are correct", {
  expect_equal(length(calculate_wads(example_qsip_object@tube_rel_abundance)), 2)
  expect_equal(dim(calculate_wads(example_qsip_object@tube_rel_abundance)$wads), c(9282, 4))
  expect_equal(dim(calculate_wads(example_qsip_object@tube_rel_abundance)$fraction), c(30450, 3))

  expect_equal(dim(calculate_source_wads(example_qsip_object@sample_data)), c(15, 2))
})
