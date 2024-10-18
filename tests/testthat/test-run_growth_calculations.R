qsip_growth_EAF <- readRDS(test_path("fixtures", "qsip_growth_EAF.rds"))

test_that("works as expected", {
  expect_snapshot(run_growth_calculations(qsip_growth_EAF,
                                       N_total_it = example_qsip_growth_t0))
})

test_that("non-qsip object errors", {
  expect_error(run_growth_calculations(example_feature_df,
                                       N_total_it = example_qsip_growth_t0),
               "object must be a <qsip_data> object, not <tbl_df>")
})

test_that("non-growth qsip object errors", {
  expect_error(run_growth_calculations(example_qsip_object,
                                          N_total_it = example_qsip_growth_t0), "timepoint column timepoint not found in source_data@data")
})


test_that("Not giving time zero (N_total_it) data failes", {
  expect_error(run_growth_calculations(qsip_growth_EAF), 'argument "N_total_it" is missing, with no default')
})


test_that("designated timepoint column not found gives error", {
  expect_error(run_growth_calculations(qsip_growth_EAF,
                                          N_total_it = example_qsip_growth_t0,
                                          timepoint = "not_found"),
               "timepoint column not_found not found in source_data@data")
})


test_that("unkonwn growth model fails", {
  expect_error(run_growth_calculations(qsip_growth_EAF,
                                       N_total_it = example_qsip_growth_t0,
                                       growth_model = "not_a_model"),
               "growth_model must be either 'exponential' or 'linear', not not_a_model")
})


test_that("unkonwn correct_copy_numbers fails", {
  expect_error(run_growth_calculations(qsip_growth_EAF,
                                       N_total_it = example_qsip_growth_t0,
                                       correct_copy_numbers = "not_an_option"),
               "correct_copy_numbers must be either 'filter' or 'adjust', not not_an_option")
})


test_that("unkonwn correct_EAF fails", {
  expect_error(run_growth_calculations(qsip_growth_EAF,
                                       N_total_it = example_qsip_growth_t0,
                                       correct_EAF = "not_an_option"),
               "correct_EAF must be either 'filter' or 'adjust', not not_an_option")
})
