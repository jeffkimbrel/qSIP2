gradient_pos_density_df <- readRDS(test_path("fixtures", "gradient_pos_density_df.rds"))
gradient_pos_density_df_fail <- readRDS(test_path("fixtures", "gradient_pos_density_df_fail.rds"))
gradient_pos_density_df_bulk <- readRDS(test_path("fixtures", "gradient_pos_density_df_bulk.rds"))

test_that("valid densities pass", {
  expect_null(validate_gradient_pos_density(gradient_pos_density_df, high = 1.8, low = 1.6))
  expect_null(validate_gradient_pos_density(gradient_pos_density_df_bulk, high = 1.8, low = 1.6))
})

test_that("densities given as strings fail", {
  expect_error(validate_gradient_pos_density(gradient_pos_density_df_fail), "some gradient_pos_density values are non-numeric")
  expect_error(validate_gradient_pos_density("not a dataframe"), "data should be class <data.frame>")
})

test_that("densities outside of the valid range fail", {
  expect_error(validate_gradient_pos_density(gradient_pos_density_df, high = 1.79), class = "qsip_density_out_of_range")
  expect_error(validate_gradient_pos_density(gradient_pos_density_df, low = 1.61), class = "qsip_density_out_of_range")
})
