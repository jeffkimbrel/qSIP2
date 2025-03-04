test_that("returns list", {
  expect_type(get_unshared_ids(example_qsip_growth_object), "list")
})

test_that("non-qsip object gives error", {
  expect_error(get_unshared_ids(example_feature_df),
               "object must be a <qsip_data> object, not <tbl_df>")
})
