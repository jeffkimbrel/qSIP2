wad_matrix <- readRDS(test_path("fixtures", "wad_matrix.rds"))

test_that("resampling works correctly", {
  set.seed(19)
  expect_snapshot(calculate_resampled_wads(1, wad_matrix, type = "labeled"))
  expect_equal(dim(calculate_resampled_wads(1, wad_matrix, type = "labeled")), c(6, 6))
  expect_equal(dim(calculate_resampled_wads(1, wad_matrix, type = "unlabeled")), c(6, 6))
})

test_that("All values must be numeric or NA", {
  expect_error(
    calculate_resampled_wads(1, data.frame("A" = c(1.1, 1.2, "not_a_number")), type = "labeled"),
    "wad dataframe to resample from contains non-numeric dat"
  )

  set.seed(200)
  expect_equal(
    dim(calculate_resampled_wads(1, data.frame("A" = c(1.1, 1.2, NA), "B" = c(2.1, 2.2, 2.3)), type = "labeled")),
    c(3, 5)
  )
})

test_that("check weird edge case where if a row is entirely <NA> it should break", {
  expect_error(calculate_resampled_wads(1, data.frame("A" = c(NA, 2.0, 1.7), "B" = c(NA, 2.2, 2.3)), type = "labeled"))
})

test_that("dataframes with only one column still complete", {
  expect_snapshot(calculate_resampled_wads(1, data.frame("A" = c(1, 2.0, 1.7)), type = "labeled"))
})
