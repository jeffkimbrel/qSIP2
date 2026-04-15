qsip_growth_rates <- readRDS(test_path("fixtures", "qsip_growth_rates.rds"))
qsip_growth_EAF <- readRDS(test_path("fixtures", "qsip_growth_EAF.rds"))

test_that("works as expected", {
  expect_snapshot(get_growth_data(qsip_growth_rates))
})


test_that("errors if given a non-growth qsip object", {
  expect_error(get_growth_data(qsip_growth_EAF), class = "qsip_wrong_state")
})

test_that("errors if given a non qsip object", {
  expect_error(get_growth_data(example_feature_df), class = "qsip_wrong_class")
})
