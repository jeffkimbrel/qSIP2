qsip_growth_rates <- readRDS(test_path("fixtures", "qsip_growth_rates.rds"))
qsip_growth_EAF <- readRDS(test_path("fixtures", "qsip_growth_EAF.rds"))

test_that("works as expected", {
  expect_snapshot(get_growth_data(qsip_growth_rates))
})


test_that("errors if given a non-growth qsip object", {
  expect_error(get_growth_data(qsip_growth_EAF), "<object> has not been run through the growth calculations")
})

test_that("errors if given a non qsip object", {
  expect_error(get_growth_data(example_feature_df), "object must be a <qsip_data> object, not <tbl_df>")
})
