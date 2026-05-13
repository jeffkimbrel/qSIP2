test_that("works as expected, or at least makes a ggplot", {
  expect_true(  expect_true(inherits(plot_sample_curves(example_qsip_growth_object), "ggplot"))
)
})

test_that("title works correctly", {
  expect_equal("test", plot_sample_curves(example_qsip_object, title = "test")$labels$title)
  expect_error(plot_sample_curves(example_qsip_object, title = example_feature_df), "title must be a character string")
})

test_that("data with unfractionated samples produce message", {
  expect_message(plot_sample_curves(example_qsip_growth_object), "some unfractionated samples have been filtered from this plot")
})

test_that("facet_by works as expected", {
  expect_error(plot_sample_curves(example_qsip_growth_object, facet_by = "not_an_option"), "not found in source metadata")
  expect_true(inherits(plot_sample_curves(example_qsip_growth_object, facet_by = "source"), "ggplot"))
  expect_true(inherits(plot_sample_curves(example_qsip_growth_object, facet_by = "isotope"), "ggplot"))
})

test_that("facet_by works with metadata columns", {
  expect_true(inherits(plot_sample_curves(example_qsip_object, facet_by = "Moisture"), "ggplot"))
  expect_true(inherits(plot_sample_curves(example_qsip_object, facet_by = c("Moisture", "isotopolog")), "ggplot"))
})

test_that("facet_by with multiple columns combines them", {
  p <- plot_sample_curves(example_qsip_object, facet_by = c("Moisture", "isotopolog"))
  expect_true(inherits(p, "ggplot"))
})

test_that("show_wad should be a boolean", {
  expect_error(plot_sample_curves(example_qsip_growth_object, show_wad = "not_an_option"), "show_wad must be TRUE/FALSE")
})
