test_that("incorrect input types give errors", {
  expect_error(
    qsip_feature_data(example_feature_object, feature_id = "ASV"),
    class = "qsip_wrong_class"
  )
  expect_error(
    qsip_feature_data(example_feature_df, feature_id = "not_found"),
    class = "qsip_column_not_found"
  )
  expect_error(
    qsip_feature_data(example_feature_df, feature_id = "ASV", type = "not_a_valid_type"),
    class = "qsip_invalid_argument"
  )
})

test_that("@data slot is the right size", {
  expect_equal(
    dim(qsip_feature_data(example_feature_df, feature_id = "ASV")@data),
    dim(example_feature_df)
  )
})

test_that("@taxonomy slot is the right size", {
  expect_equal(
    dim(qsip_feature_data(example_feature_df, feature_id = "ASV")@taxonomy),
    dim(data.frame())
  )
})

test_that("Duplicate feature ids give error", {
  test_df <- example_feature_df
  test_df$ASV <- sample(test_df$ASV, replace = T)
  expect_error(
    qsip_feature_data(test_df, feature_id = "ASV"),
    class = "qsip_duplicate_ids"
  )
})
