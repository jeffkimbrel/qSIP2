df = get_comparison_groups(example_qsip_object, group = "Moisture") |>
  dplyr::select("group" = Moisture, "unlabeled" = "12C", "labeled" = "13C")

test_that("works as expected", {
  expect_snapshot(run_comparison_groups(df, example_qsip_object,
                                        seed = 99,
                                        allow_failures = TRUE,
                                        resamples = 100))
})

test_that("wrong qsip_object gives error", {
  expect_error(run_comparison_groups(df, "a string"),
               class = "qsip_wrong_class")
})

test_that("missing expected column names fail", {
  expect_error(run_comparison_groups(dplyr::rename(df, BAD_NAME = group), example_qsip_object),
               class = "qsip_column_not_found")
  expect_error(run_comparison_groups(dplyr::rename(df, BAD_NAME = unlabeled), example_qsip_object),
               class = "qsip_column_not_found")
  expect_error(run_comparison_groups(dplyr::rename(df, BAD_NAME = labeled), example_qsip_object),
               class = "qsip_column_not_found")
})

test_that("group names not unique", {
  expect_error(run_comparison_groups(rbind(df, df), example_qsip_object),
               "group column must be unique")
})

test_that("group names must be found in object", {
  expect_error(run_comparison_groups(df |> dplyr::mutate(unlabeled = gsub("S", "X", unlabeled)),
    example_qsip_object),
               "Invalid source_mat_ids in group dataframe")
  expect_error(run_comparison_groups(df |> dplyr::mutate(labeled = gsub("S", "X", labeled)),
                                     example_qsip_object),
               "Invalid source_mat_ids in group dataframe")
})




# df |> dplyr::mutate(unlabeled = gsub("S", "X", unlabeled))
