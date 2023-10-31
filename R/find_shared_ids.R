#' Find shared sources and samples in qSIP objects
#'
#' @param source_data qsip source_data object
#' @param sample_data qsip sample_data object
#' @param feature_data qsip feature_data object
#'
#' @export


find_shared_ids <- function(source_data, sample_data, feature_data) {
  missing <- FALSE

  # Report the source ids shared and unique
  source_source_mat_id <- source_data@data |>
    dplyr::pull(source_mat_id) |>
    unique()
  sample_source_mat_id <- sample_data@data |>
    dplyr::pull(source_mat_id) |>
    unique()

  shared <- list(
    "sources" = list("shared" = c(), "source_data" = c(), "sample_data" = c()),
    "samples" = list("shared" = c(), "sample_data" = c(), "feature_id" = c())
  )

  shared$sources$shared <- intersect(source_source_mat_id, sample_source_mat_id)

  if (isTRUE(setequal(source_source_mat_id, sample_source_mat_id))) {
    message(glue::glue_col("{green There are {length(source_source_mat_id)} source_mat_ids, and they are all shared between the source and sample objects}"))
  } else {
    if (length(setdiff(source_source_mat_id, sample_source_mat_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: source_data has {length(setdiff(source_source_mat_id, sample_source_mat_id))} source_mat_id(s) that are missing from the sample_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(source_source_mat_id, sample_source_mat_id), collapse = ', ')}}"))
      shared$sources$source_data <- setdiff(source_source_mat_id, sample_source_mat_id)
      missing <- TRUE
    }

    if (length(setdiff(sample_source_mat_id, source_source_mat_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: sample_data has {length(setdiff(sample_source_mat_id, source_source_mat_id))} source_mat_id(s) that are missing from the source_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(sample_source_mat_id, source_source_mat_id), collapse = ', ')}}"))
      shared$sources$sample_data <- setdiff(sample_source_mat_id, source_source_mat_id)
      missing <- TRUE
    }
  }

  # Report the sample ids shared and unique
  sample_sample_id <- sample_data@data |>
    dplyr::pull(sample_id) |>
    unique()
  feature_sample_id <- feature_data@data |>
    dplyr::select(-feature_id) %>%
    colnames()

  shared$samples$shared <- intersect(sample_sample_id, feature_sample_id)
  if (isTRUE(setequal(sample_sample_id, feature_sample_id))) {
    message(glue::glue_col("{green There are {length(sample_sample_id)} feature_ids, and they are all shared between the sample and feature objects}"))
  } else {
    if (length(setdiff(sample_sample_id, feature_sample_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: sample_data has {length(setdiff(sample_sample_id, feature_sample_id))} feature_id(s) that are missing from the feature_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(sample_sample_id, feature_sample_id), collapse = ', ')}}"))
      shared$samples$sample_data <- setdiff(sample_sample_id, feature_sample_id)
      missing <- TRUE
    }

    if (length(setdiff(feature_sample_id, sample_sample_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: feature_data has {length(setdiff(feature_sample_id, sample_sample_id))} feature_id(s) that are missing from the sample_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(feature_sample_id, sample_sample_id), collapse = ', ')}}"))
      shared$samples$feature_id <- setdiff(feature_sample_id, sample_sample_id)
      missing <- TRUE
    }
  }

  if (isTRUE(missing)) {
    message()
    message(glue::glue_col("{yellow ***Missing sources/samples have not been removed from the dataset***}"))
    message(glue::glue_col("{yellow ***Run show_unshared_ids(<qsip_data_object>) to show IDs missing from datasets***}"))  }

  return(shared)
}
