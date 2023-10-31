qsip_sample_data_test_df <- example_sample_df |>
  add_gradient_pos_rel_amt(source_mat_id = "source", amt = "avg_16S_g_soil") |>
  dplyr::mutate(Fraction = as.integer(Fraction))

test_that("works correctly", {
  expect_snapshot(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ))
})

test_that("missing columns give errors", {
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "not_found",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "ERROR: sample_id column missing")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "not_found",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "ERROR: source_mat_id column missing")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "not_found",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "ERROR: gradient_position column missing")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "not_found",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "ERROR: gradient_pos_density column missing")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "not_found",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "ERROR: gradient_pos_amt column missing")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "not_found"
  ), "ERROR: gradient_pos_rel_amt column missing")
})
