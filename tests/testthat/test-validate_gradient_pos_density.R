df = tibble::tibble("gradient_position" = c(1, 2, 3),
                             "gradient_pos_density" = c(1.6, 1.7, 1.8))

df_fail = tibble::tibble("gradient_position" = c(1, 2, 3),
                    "gradient_pos_density" = c("1.6", "1.7", "1.8"))

df_bulk = tibble::tibble("gradient_position" = c(-1, 2, 3),
                    "gradient_pos_density" = c(1.6, 1.7, 1.8))

test_that("valid densities pass", {
  expect_null(validate_gradient_pos_density(df, high = 1.8, low = 1.6))
  expect_null(validate_gradient_pos_density(df_bulk, high = 1.8, low = 1.6))
})

test_that("densities given as strings fail", {
  expect_error(validate_gradient_pos_density(df_fail), "some gradient_pos_density values are non-numeric")
  expect_error(validate_gradient_pos_density("not a dataframe"), "data should be class <data.frame>")
})

test_that("densities outside of the valid range fail", {
  expect_error(validate_gradient_pos_density(df, high = 1.79), "some gradient_pos_density values are higher than 1.79")
  expect_error(validate_gradient_pos_density(df, low = 1.61), "some gradient_pos_density values are lower than 1.61")
})
