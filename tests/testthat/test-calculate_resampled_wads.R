test_wad <- data.frame(labeled_1 = c(
  1.70130927995061, 1.71842281966778, 1.73540734122641,
  1.72122157361111, 1.7121411962124, 1.73366339148623
), labeled_2 = c(
  1.70489583061372,
  1.72034693345524, 1.73618421172493, 1.7294567555353, 1.71751884929873,
  1.73397109427623
), labeled_3 = c(
  1.70130927995061, 1.71842281966778,
  1.73540734122641, 1.72122157361111, 1.7121411962124, 1.73366339148623
))

test_that("resampling works correctly", {
  set.seed(19)
  expect_snapshot(calculate_resampled_wads(1, test_wad, type = "labeled"))
  expect_equal(dim(calculate_resampled_wads(1, test_wad, type = "labeled")), c(6, 6))
  expect_equal(dim(calculate_resampled_wads(1, test_wad, type = "unlabeled")), c(6, 6))
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

