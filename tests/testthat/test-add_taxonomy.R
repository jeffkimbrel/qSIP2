add_taxonomy_testdf <- dplyr::select(example_feature_object@data, feature_id) |>
  dplyr::mutate(genus = "test", species = "test")


test_that("feature wrong type produces error", {
  expect_error(add_taxonomy(example_feature_df, add_taxonomy_testdf, feature_id = "ASV"),
               "feature_object should be class <qsip_feature_data>, not tbl_df)")
})

test_that("feature column not found produce error", {
  expect_error(add_taxonomy(example_feature_object, add_taxonomy_testdf, feature_id = "not_found"),
               "not_found column not found in taxonomy dataframe")
})

test_that("duplicate feature_ids produce error", {
  expect_error(add_taxonomy(example_feature_object,
    dplyr::sample_n(add_taxonomy_testdf, size = nrow(add_taxonomy_testdf), replace = T),
    feature_id = "feature_id"
  ), "some feature_ids in the taxonomy dataframe are duplicated")
})

test_that("features missing from taxonomy table give error", {
  expect_error(add_taxonomy(example_feature_object,
    dplyr::sample_n(add_taxonomy_testdf, size = 10),
    feature_id = "feature_id"
  ), "Some ids found in the abundance object are not found in the taxa table")
})

test_that("extra features in taxonomy table give error", {
  expect_error(add_taxonomy(example_feature_object,
    rbind(add_taxonomy_testdf, add_taxonomy_testdf),
    feature_id = "feature_id"
  ), "some feature_ids in the taxonomy dataframe are duplicated")
})

test_that("@taxonomy slot gets populated correctly", {
  expect_equal(
    dim(add_taxonomy(example_feature_object, add_taxonomy_testdf, feature_id = "feature_id")@taxonomy),
    c(2030, 3)
  )
})
