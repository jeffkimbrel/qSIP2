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

test_that("Missing column names produce error", {
  expect_error(
    add_gradient_pos_rel_amt(
      example_sample_df,
      amt = "not_found",
      source_mat_id = "source"
    )
  )

  expect_error(
    add_gradient_pos_rel_amt(
      example_sample_df,
      amt = "avg_16S_g_soil",
      source_mat_id = "not_found"
    )
  )
})


test_that("Overwite = T throws error, overwite = F doesn't", {
  test_df = add_gradient_pos_rel_amt(
    example_sample_df,
    amt = "avg_16S_g_soil",
    source_mat_id = "source"
  )

  expect_error(
    add_gradient_pos_rel_amt(
      test_df,
      amt = "avg_16S_g_soil",
      source_mat_id = "source"
    )
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
