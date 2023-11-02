test_that("data not a dataframe throws error", {
  expect_error(
    qsip_source_data(example_source_object),
    "data should be class <data.frame>"
  )
})

test_that("Missing columns throw errors", {
  df <- example_source_df |>
    dplyr::mutate(isotopolog = "glucose")

  expect_error(
    qsip_source_data(df, isotope = "not_found", isotopolog = "isotopolog", source_mat_id = "source"),
    "isotope column 'not_found' is not found"
  )

  expect_error(
    qsip_source_data(df, isotope = "Isotope", isotopolog = "not_found", source_mat_id = "source"),
    "isotopolog column 'not_found' is not found"
  )

  expect_error(
    qsip_source_data(df, isotope = "Isotope", isotopolog = "isotopolog", source_mat_id = "not_found"),
    "source_mat_id column 'not_found' is not found"
  )
})

test_that("Duplicate source ids give error", {
  expect_error(
    example_source_df |>
      dplyr::mutate(isotopolog = "glucose") |>
      dplyr::mutate(source = sample(source, replace = T)) |> # duplicates by sampling with replacement
      qsip_source_data(
        isotope = "Isotope",
        isotopolog = "isotopolog",
        source_mat_id = "source"
      ), "some source_mat_ids are duplicated"
  )
})


test_that("Invalid isotopes give error", {
  expect_error(
    example_source_df |>
      dplyr::mutate(isotopolog = "glucose") |>
      dplyr::mutate(Isotope = "not_an_isotope") |>
      qsip_source_data(
        isotope = "Isotope",
        isotopolog = "isotopolog",
        source_mat_id = "source"
      ), "Please fix the isotope names and try again"
  )
})
