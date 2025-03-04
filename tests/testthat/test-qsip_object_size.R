l = list("A" = example_qsip_object, "B" = example_qsip_growth_object)

test_that("size of list looks OK", {
  expect_snapshot(qsip_object_size(l))
})

test_that("size of single object looks OK", {
  expect_snapshot(qsip_object_size(example_qsip_object))
})

test_that("non-qsip object errors", {
  expect_error(qsip_object_size(example_feature_df), "<qsip_data_object> must be a qsip object or list of qsip objects")
})
