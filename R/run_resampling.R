#' Resample WAD values
#'
#' Takes a filtered WAD dataframe and resamples x times
#'
#' This function returns a list of resampled dataframes of x length for both the
#' labeled and unlabeled sources.
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been filtered
#' @param resamples (*integer*) The number of resamples/bootstraps to run
#' @param with_seed (*integer*) An optional seed for reproducibility
#'
#' @returns A new `qsip_data` object with the `@resamples` slot populated with resamples wad values
#'
#' @export

run_resampling <- function(qsip_data_object,
                           resamples = 1000,
                           with_seed = NULL) {
  if (length(qsip_data_object@filtered_wad_data) == 0) {
    stop("ERROR: this function requires a qsip object that has been run through run_feature_filter() ")
  }




  unlabeled <- qsip_data_object@filter_results$unlabeled_source_mat_ids
  labeled <- qsip_data_object@filter_results$labeled_source_mat_ids


  # set seed if given
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # create individual dataframes for resampling
  labeled_wads <- qsip_data_object@filtered_wad_data |>
    tibble::column_to_rownames("feature_id") |>
    dplyr::select(dplyr::all_of(labeled))

  unlabeled_wads <- qsip_data_object@filtered_wad_data |>
    tibble::column_to_rownames("feature_id") |>
    dplyr::select(dplyr::all_of(unlabeled))

  # remove NAs?

  # run resampling. This will return a list of x size with resampling results
  labeled_resamples <- purrr::map(1:resamples, \(i) calculate_resampled_wads(
    i, labeled_wads,
    "labeled"
  ), .progress = "labeled resamples...")
  unlabeled_resamples <- purrr::map(1:resamples, \(i) calculate_resampled_wads(
    i, unlabeled_wads,
    "unlabeled"
  ), .progress = "unlabeled resamples...")

  # merge two lists into a list of lists in the @resamples slot
  qsip_data_object@resamples <- list(
    "l" = labeled_resamples,
    "u" = unlabeled_resamples
  )

  # return the complete qsip_data object
  qsip_data_object
}
