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
    "ERROR: qsip_data_object must be of type qsip_data or qsip_source_data"
  )
})


test_that("multiple isotopes work", {
  expect_snapshot(get_all_by_isotope(example_qsip_object, c("12C", "13C")))
})

test_that("no hits returned gives an error", {
  expect_error(
    get_all_by_isotope(example_qsip_object, "14N"),
    "ERROR: No source_mat_ids found with isotopes 14N"
  )
})

test_that("some isotopes not found give a message, but don't fail", {
  expect_message(
    get_all_by_isotope(example_qsip_object, c("12C", "14N")),
    "WARNING: 14N not found in data"
  )
})
