test_that("dimensions of returned data are correct", {
  expect_snapshot(calculate_wads(example_qsip_object@tube_rel_abundance))
  expect_equal(length(calculate_wads(example_qsip_object@tube_rel_abundance)), 2)
  expect_equal(dim(calculate_wads(example_qsip_object@tube_rel_abundance)$wads), c(9282, 4))
  expect_equal(dim(calculate_wads(example_qsip_object@tube_rel_abundance)$fraction), c(30450, 3))

  expect_equal(dim(source_wads(example_qsip_object)), c(15, 2))
})


test_that("wrong table sent gives error", {
  expect_error(calculate_wads(example_qsip_object), "data is not class data.frame")
  expect_error(calculate_wads(example_qsip_object@tube_rel_abundance |> dplyr::select(-feature_id)),
               "feature_id column missing")
  expect_error(calculate_wads(example_qsip_object@tube_rel_abundance |> dplyr::select(-source_mat_id)),
               "source_mat_id column missing")
  expect_error(calculate_wads(example_qsip_object@tube_rel_abundance |> dplyr::select(-tube_rel_abundance)),
               "tube_rel_abundance column missing")
  expect_error(calculate_wads(example_qsip_object@tube_rel_abundance |> dplyr::select(-gradient_pos_density)),
               "gradient_pos_density column missing")
})

