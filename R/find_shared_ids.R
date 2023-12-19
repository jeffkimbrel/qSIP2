#' Find shared source_mat_ids and sample_ids in qSIP objects (internal)
#'
#' @description
#' This function finds the shared source_mat_ids between the `source_data` and
#' `sample_data`, and sample_ids between the `sample_data` and `feature_data` objects.
#' It also reports any ids that are unique to each object.
#'
#' If passing a `qsip_data` object as the first argument, no other arguments are
#' necessary. If a `qsip_source_data` is given, then the `sample_data` and `feature_data`
#' objects must be given as well.
#'
#' Additionally, the results of `find_share_ids()` might be obtained from the `@shared`
#' slot of a `qsip_data` object. This data can also be "pretty printed" using the
#' `show_unshared_ids(<qsip_data>)` function.
#'
#' @param source_data (*qsip_source_data or qsip_data*) A qSIP object with source data object
#' @param sample_data (*qsip_sample_data*) A qSIP sample data object
#' @param feature_data (*qsip_feature_data*) A qSIP feature data object
#'
#' @export
#'
#' @returns A list with two lists, one for source_mat_ids and one for sample_ids.

find_shared_ids <- function(source_data, sample_data = NULL, feature_data = NULL) {

  # Check that the source_data object is a qsip_source_data or qsip_data object
  if (!is(source_data, "qsip_source_data") && !is(source_data, "qsip_data")) {
    stop(glue::glue("source_data must be a <qsip_source_data> or <qsip_data object>, not <{class(source_data)[1]}>"))
  }

  if (is(source_data, "qsip_data")) {
    feature_data <- source_data@feature_data
    sample_data <- source_data@sample_data
    source_data <- source_data@source_data # after getting the first two objects, overwrite the source_data object
  } else {
    # make sure sample_data is a qsip_sample_data object and feature_data is a qsip_feature_data object
    if (!is(sample_data, "qsip_sample_data")) {
      stop(glue::glue("sample_data must be a <qsip_sample_data> object, not <{class(sample_data)[1]}>"))
    }

    if (!is(feature_data, "qsip_feature_data")) {
      stop(glue::glue("feature_data must be a <qsip_feature_data> object, not <{class(feature_data)[1]}>"))
    }
  }

  missing <- FALSE

  # Report the source ids shared and unique
  source_source_mat_id <- source_data@data |>
    dplyr::pull(source_mat_id) |>
    unique()

  sample_source_mat_id <- sample_data@data |>
    dplyr::pull(source_mat_id) |>
    unique()

  shared <- list(
    "source_mat_ids" = list("shared" = c(), "source_data" = c(), "sample_data" = c()),
    "sample_ids" = list("shared" = c(), "sample_data" = c(), "feature_data" = c())
  )

  shared$source_mat_ids$shared <- intersect(source_source_mat_id, sample_source_mat_id)

  if (isTRUE(setequal(source_source_mat_id, sample_source_mat_id))) {
    message(glue::glue_col("{green There are {length(source_source_mat_id)} source_mat_ids, and they are all shared between the source and sample objects}"))
  } else {
    if (length(setdiff(source_source_mat_id, sample_source_mat_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: source_data has {length(setdiff(source_source_mat_id, sample_source_mat_id))} source_mat_id(s) that are missing from the sample_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(source_source_mat_id, sample_source_mat_id), collapse = ', ')}}"))
      shared$source_mat_ids$source_data <- setdiff(source_source_mat_id, sample_source_mat_id)
      missing <- TRUE
    }

    if (length(setdiff(sample_source_mat_id, source_source_mat_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: sample_data has {length(setdiff(sample_source_mat_id, source_source_mat_id))} source_mat_id(s) that are missing from the source_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(sample_source_mat_id, source_source_mat_id), collapse = ', ')}}"))
      shared$source_mat_ids$sample_data <- setdiff(sample_source_mat_id, source_source_mat_id)
      missing <- TRUE
    }
  }

  # Report the sample ids shared and unique
  sample_sample_id <- sample_data@data |>
    dplyr::pull(sample_id) |>
    unique()

  feature_sample_id <- feature_data@data |>
    dplyr::select(-feature_id) |>
    colnames()

  shared$sample_ids$shared <- intersect(sample_sample_id, feature_sample_id)
  if (isTRUE(setequal(sample_sample_id, feature_sample_id))) {
    message(glue::glue_col("{green There are {length(sample_sample_id)} sample_ids, and they are all shared between the sample and feature objects}"))
  } else {
    if (length(setdiff(sample_sample_id, feature_sample_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: sample_data has {length(setdiff(sample_sample_id, feature_sample_id))} sample_id(s) that are missing from the feature_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(sample_sample_id, feature_sample_id), collapse = ', ')}}"))
      shared$sample_ids$sample_data <- setdiff(sample_sample_id, feature_sample_id)
      missing <- TRUE
    }

    if (length(setdiff(feature_sample_id, sample_sample_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: feature_data has {length(setdiff(feature_sample_id, sample_sample_id))} sample_id(s) that are missing from the sample_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(feature_sample_id, sample_sample_id), collapse = ', ')}}"))
      shared$sample_ids$feature_data <- setdiff(feature_sample_id, sample_sample_id)
      missing <- TRUE
    }
  }

  if (isTRUE(missing)) {
    message()
    message(glue::glue_col("{yellow ***Missing source_mat_ids/sample_ids have not been removed from the dataset***}"))
    message(glue::glue_col("{yellow ***Run show_unshared_ids(<qsip_data_object>) to show IDs missing from datasets***}"))  }

  return(shared)
}
