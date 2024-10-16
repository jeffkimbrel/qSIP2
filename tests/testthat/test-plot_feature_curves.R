test_that("works as expected, or at least makes a ggplot", {
  expect_true("ggplot" %in% class(plot_feature_curves(example_qsip_object, feature_ids = "ASV_2")))
})

test_that("source_mat_id selection works", {
  expect_no_error(plot_feature_curves(example_qsip_object,
                                  source_mat_ids = "S152",
                                  feature_ids = "ASV_2"))
  expect_no_error(plot_feature_curves(example_qsip_object,
                                      source_mat_ids = NULL,
                                      feature_ids = "ASV_2"))
  expect_error(plot_feature_curves(example_qsip_object,
                                      source_mat_ids = "not_an_ID",
                                      feature_ids = "ASV_2"),
               "ome provided source_mat_ids are not found in the qsip_data object")
})


test_that("feature_id selection works", {
  expect_no_error(plot_feature_curves(example_qsip_object,
                                      feature_ids = c("ASV_2", "ASV_5")))
  expect_error(plot_feature_curves(example_qsip_object), 'argument "feature_ids" is missing, with no default')
  expect_error(plot_feature_curves(example_qsip_object,
                                   feature_ids = "not_an_ID"),
               "some provided feature_ids are not found in the qsip_data object")
})

test_that("titling works", {
  p <- plot_feature_curves(example_qsip_object,
                           feature_ids = c("ASV_2", "ASV_5"))
  expect_null(p$labels$title)

  p <- plot_feature_curves(example_qsip_object,
                           feature_ids = c("ASV_2", "ASV_5"),
                           title = "My Title")
  expect_equal(p$labels$title, "My Title")
})
