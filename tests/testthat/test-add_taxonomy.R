add_taxonomy_testdf <- dplyr::select(example_feature_object@data, feature_id) |>
  dplyr::mutate(genus = "test", species = "test")

test_that("feature column not found produce error", {
  expect_error(add_taxonomy(example_feature_object, add_taxonomy_testdf, feature_id = "not_found"))
})

test_that("duplicate feature_ids produce error", {
  expect_error(add_taxonomy(example_feature_object,
    dplyr::sample_n(add_taxonomy_testdf, size = nrow(add_taxonomy_testdf), replace = T),
    feature_id = "feature_id"
  ))
})

test_that("features missing from taxonomy table give error", {
  expect_error(add_taxonomy(example_feature_object,
    dplyr::sample_n(add_taxonomy_testdf, size = 10),
    feature_id = "feature_id"
  ))
})

test_that("extra features in taxonomy table give error", {
  expect_error(add_taxonomy(example_feature_object,
    rbind(add_taxonomy_testdf, add_taxonomy_testdf),
    feature_id = "feature_id"
  ))
})

test_that("@taxonomy slot gets populated correctly", {
  expect_equal(
    dim(add_taxonomy(example_feature_object, add_taxonomy_testdf, feature_id = "feature_id")@taxonomy),
    c(2030, 3)
  )
})
