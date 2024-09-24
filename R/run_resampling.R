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
#' @param progress (*boolean*) Option to show a progress bar for the resampling step
#' @param allow_failures (*boolean*) Option to allow resampling failures. If TRUE, the function will continue to resample even if some features fail. If FALSE, the function will stop if any features fail.
#' @param quiet (*boolean*) Option to suppress messages
#'
#' @returns A new `qsip_data` object with the `@resamples` slot populated with resamples wad values
#'
#' @export

run_resampling <- function(qsip_data_object,
                           resamples = 1000,
                           with_seed = NULL,
                           allow_failures = FALSE,
                           progress = TRUE,
                           quiet = FALSE) {
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  } else if (length(qsip_data_object@filtered_wad_data) == 0) {
    stop("ERROR: this function requires a qsip object that has been run through run_feature_filter()")
  }

  stopifnot("progress must be either TRUE of FALSE" = progress %in% c(TRUE, FALSE))
  stopifnot("resamples should be class <numeric>" = is.numeric(resamples))
  stopifnot("resamples should be positive" = resamples > 0)

  # bind variables
  n <- type <- NULL

  # v0.11.5 fixed to convert to characters
  unlabeled <- as.character(qsip_data_object@filter_results$unlabeled_source_mat_ids)
  labeled <- as.character(qsip_data_object@filter_results$labeled_source_mat_ids)


  # set seed if given
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  } else {
    if (isFALSE(quiet)) {
      message("Using random seed. For consistency, you can use the with_seed argument")
    }
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
  if (isTRUE(progress)) {
    progress_bar_l <- "labeled resamples..."
    progress_bar_u <- "unlabeled resamples..."
  } else {
    progress_bar_l <- FALSE
    progress_bar_u <- FALSE
  }

  labeled_resamples <- purrr::map(1:resamples, \(i) calculate_resampled_wads(
    i, labeled_wads,
    "labeled",
    allow_failures = allow_failures
  ), .progress = progress_bar_l)

  unlabeled_resamples <- purrr::map(1:resamples, \(i) calculate_resampled_wads(
    i, unlabeled_wads,
    "unlabeled",
    allow_failures = allow_failures
  ), .progress = progress_bar_u)

  # merge two lists into a list of lists in the @resamples slot
  qsip_data_object@resamples <- list(
    "l" = labeled_resamples,
    "u" = unlabeled_resamples,
    "n" = resamples,
    "seed" = with_seed,
    "allow_failures" = allow_failures
  )

  if (isTRUE(allow_failures)) {
    failures <- get_resample_counts(qsip_data_object) |>
      tidyr::pivot_longer(cols = c("unlabeled_resamples", "labeled_resamples"),
                          names_to = "type",
                          values_to = "n") |>
      dplyr::filter(n < 1000) |>
      dplyr::group_by(type) |>
      dplyr::count() |>
      tibble::deframe()

    if (sum(failures > 0)) {
      if (isFALSE(quiet)) {
        warning(glue::glue("{failures['unlabeled']} unlabeled and {failures['labeled']} labeled feature_ids had resampling failures. Run `get_resample_counts()` or `plot_successful_resamples()` on your <qsip_data> object to inspect."),
                call. = FALSE)
      }
    }
  }



  # return the complete qsip_data object
  qsip_data_object
}
