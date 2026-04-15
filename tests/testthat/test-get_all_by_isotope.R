test_that("correct input types work", {
  expect_snapshot(get_all_by_isotope(example_qsip_object, "12C"))
  expect_snapshot(get_all_by_isotope(example_source_object, "12C"))
  expect_snapshot(get_all_by_isotope(example_qsip_object, "13C"))
  expect_snapshot(get_all_by_isotope(example_source_object, "13C"))
  expect_snapshot(get_all_by_isotope(example_qsip_object, "labeled"))
  expect_snapshot(get_all_by_isotope(example_qsip_object, "unlabeled"))
})

test_that("incorrect input type fails", {
  expect_error(
    get_all_by_isotope(example_feature_object, "12C"),
    "qsip_data_object must be class <qsip_data> or <qsip_source_data>"
  )
})


test_that("multiple isotopes work", {
  expect_snapshot(get_all_by_isotope(example_qsip_object, c("12C", "13C")))
})

test_that("no hits returned gives an error", {
  expect_error(
    get_all_by_isotope(example_qsip_object, "14N"),
    class = "qsip_isotope_not_found_in_source_mat_ids"
  )
})

test_that("some isotopes not found give a warning, but don't fail", {
  expect_warning(
    get_all_by_isotope(example_qsip_object, c("12C", "14N")),
    class = "qsip_isotope_not_found"
  )
})

