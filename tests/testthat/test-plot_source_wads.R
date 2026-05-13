test_that("gives ggplot object", {
  expect_true("ggplot" %in% class(plot_source_wads(example_qsip_object)))
  expect_true("ggplot" %in% class(plot_source_wads(example_qsip_object, title = "A title")))
})


test_that("gives error if not qsip object", {
  expect_error(plot_source_wads(example_feature_df),
            "object must be a <qsip_data> object, not <tbl_df>")
})

test_that("gives error if group is not a column name in source_data", {
  expect_no_error(plot_source_wads(example_qsip_object, group = "Moisture"))
  expect_error(plot_source_wads(example_qsip_object, group = "not_a_column"),
            "not found in source metadata")
})

test_that("group works with multiple columns", {
  expect_true(inherits(plot_source_wads(example_qsip_object, group = c("Moisture", "isotopolog")), "ggplot"))
})

test_that("titling works", {
  expect_no_error(plot_source_wads(example_qsip_object, title = "A title"))
  expect_error(plot_source_wads(example_qsip_object, group = "not_a_column"),
            "not found in source metadata")

  p <- plot_source_wads(example_qsip_object)
  expect_null(p$labels$title)

  p <- plot_source_wads(example_qsip_object,
                           title = "My Title")
  expect_equal(p$labels$title, "My Title")
})
