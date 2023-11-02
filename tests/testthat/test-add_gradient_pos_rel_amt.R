test_that("Returns data.frame", {
  expect_s3_class(
    add_gradient_pos_rel_amt(
      example_sample_df,
      amt = "avg_16S_g_soil",
      source_mat_id = "source"
    ),
    "data.frame"
  )
})

test_that("works correctly", {
  expect_snapshot(add_gradient_pos_rel_amt(
    example_sample_df,
    amt = "avg_16S_g_soil",
    source_mat_id = "source"
  ))
})

test_that("Missing column names produce error", {
  expect_error(
    add_gradient_pos_rel_amt(
      example_sample_df,
      amt = "not_found",
      source_mat_id = "source"
    ), "not_found not found in dataframe"
  )
  expect_error(
    add_gradient_pos_rel_amt(
      example_sample_df,
      amt = "avg_16S_g_soil",
      source_mat_id = "not_found"
    ), "not_found not found in dataframe"
  )
})


test_that("Overwite = T throws error, overwite = F doesn't", {
  test_df <- add_gradient_pos_rel_amt(
    example_sample_df,
    amt = "avg_16S_g_soil",
    source_mat_id = "source"
  )

  expect_error(
    add_gradient_pos_rel_amt(
      test_df,
      amt = "avg_16S_g_soil",
      source_mat_id = "source"
    ), "gradient_pos_rel_amt already exists! Set overwrite = TRUE if you want to overwrite"
  )

  expect_s3_class(
    add_gradient_pos_rel_amt(
      test_df,
      amt = "avg_16S_g_soil",
      source_mat_id = "source",
      overwrite = T
    ),
    "data.frame"
  )
})
