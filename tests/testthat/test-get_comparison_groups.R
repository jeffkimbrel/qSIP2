test_that("Works as expected", {
  expect_snapshot(get_comparison_groups(example_qsip_object, group = "Moisture"))
  expect_snapshot(get_comparison_groups(example_source_object, group = "Moisture"))
  expect_snapshot(get_comparison_groups(example_source_df,
    group = "Moisture",
    source_mat_id = "source",
    isotope = "Isotope"
  ))
})

test_that("Wrong input types fail", {
  expect_error(
    get_comparison_groups(example_feature_df),
    "ERROR: Please provide a grouping variable with the 'group' argument"
  )
  expect_error(
    get_comparison_groups(group = "Moisture"),
    "ERROR: Please provide source data with the 'source_data' argument."
  )
  expect_error(
    get_comparison_groups(example_feature_df, group = "Moisture"),
    "ERROR: Please provide the column name with the source_mat_id"
  )
  expect_error(get_comparison_groups(example_source_df,
    group = "Moisture",
    source_mat_id = "not_found",
    isotope = "Isotope"
  ), "ERROR: Please provide the column name with the source_mat_id")
  expect_error(get_comparison_groups(example_source_df,
    group = "Moisture",
    source_mat_id = "source",
    isotope = "not_found"
  ), "ERROR: Please provide the column name with isotope data")
  expect_error(get_comparison_groups(example_source_df,
    group = "not_found",
    source_mat_id = "source",
    isotope = "Isotope"
  ), "ERROR: grouping column 'not_found' not found")
})
