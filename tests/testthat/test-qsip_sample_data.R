qsip_sample_data_test_df <- example_sample_df |>
  add_gradient_pos_rel_amt(source_mat_id = "source", amt = "avg_16S_g_soil") |>
  dplyr::mutate(Fraction = as.integer(Fraction))

qsip_sample_data_test_df_no_rel <- example_sample_df |>
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

test_that("no gradient_pos_rel_amt given will autocalculate", {
  expect_snapshot(qsip_sample_data(qsip_sample_data_test_df_no_rel,
                                   sample_id = "sample",
                                   source_mat_id = "source",
                                   gradient_position = "Fraction",
                                   gradient_pos_density = "density_g_ml",
                                   gradient_pos_amt = "avg_16S_g_soil"
  ))
  expect_message(qsip_sample_data(qsip_sample_data_test_df_no_rel,
                                   sample_id = "sample",
                                   source_mat_id = "source",
                                   gradient_position = "Fraction",
                                   gradient_pos_density = "density_g_ml",
                                   gradient_pos_amt = "avg_16S_g_soil"),
                 "<gradient_pos_rel_amt> not specified. Calculating using avg_16S_g_soil column")
})

test_that("missing columns give errors", {
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "not_found",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "sample_id column not found")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "not_found",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "source_mat_id column not found")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "not_found",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "gradient_position column not found")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "not_found",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "gradient_pos_density column not found")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "not_found",
    gradient_pos_rel_amt = "gradient_pos_rel_amt"
  ), "gradient_pos_amt column not found")
  expect_error(qsip_sample_data(qsip_sample_data_test_df,
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil",
    gradient_pos_rel_amt = "not_found"
  ), "gradient_pos_rel_amt column not found")
})
