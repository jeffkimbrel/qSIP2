#' Filter features in a qSIP data object
#'
#' Filters a feature in a feature table in a `qsip_data` object by presence in
#' a minimum number of fractions and source_mat_ids.
#'
#' Filtering is first done on the fractions, and then on the source_mat_ids. For
#' example, take a feature that is found in three source_mat_ids in 3, 5 and 9
#' fractions. If you set `min_fractions = 5` and `min_sources = 3` then this
#' feature will not survive the filtering because although it is found in three
#' source_mat_ids, one of them is less then the minimum fraction count and would
#' therefore be considered not found in that source_mat_id.
#'
#' The feature table is filtered to retain only the feature_ids passing the
#' filter, and to keep only the sample_ids that correspond to the given
#' source_mat_ids. This filtered table is stored in the `@filtered_feature_data`
#' slot, and the values in this table are not the raw initial values but are the
#' relative abundances per fraction per tube. These values are originally
#' calculated during `qsip_data` object creation and the values for all features
#' are stored in the `@tube_rel_abundance` slot.
#'
#' @param qsip_data_object (*qsip_data*) An object of `qsip_data` class
#' @param group (*string*) An optional name to assign to this filtered group
#' @param unlabeled_source_mat_ids (*string or strings(s)*) A list of the unlabeled source_mat_ids to filter on
#' @param labeled_source_mat_ids (*string or strings(s)*) A list of the labeled source_mat_ids to filter on
#' @param min_unlabeled_sources (*integer, default: 2*) Minimum number of unlabeled source_mat_ids a feature must be found in.
#' @param min_labeled_sources (*integer, default: 2*) Minimum number of labeled source_mat_ids a feature must be found in.
#' @param min_unlabeled_fractions (*integer, default: 2*) Minimum number of fractions a feature must be found in to be present in an unlabeled source_mat_id
#' @param min_labeled_fractions (*integer, default: 2*) Minimum number of fractions a feature must be found in to be present in an labeled source_mat_id
#' @param quiet (*logical, default: FALSE*) Suppress messages
#'
#' @export
#'
#' @family "qSIP Filtering"
#'
#' @returns An updated `qsip_data` object with a filtered feature dataframe in the
#' `@filtered_feature_data` slot and intermediate data in the `@filter_results`
#' slot for plotting.

run_feature_filter <- function(qsip_data_object,
                               group = NULL,
                               unlabeled_source_mat_ids,
                               labeled_source_mat_ids,
                               min_unlabeled_sources = 2,
                               min_labeled_sources = 2,
                               min_unlabeled_fractions = 2,
                               min_labeled_fractions = 2,
                               quiet = FALSE) {

  # error if is_qsip_data(qsip_data_object) is not true
  if (isFALSE(is_qsip_data(qsip_data_object))) {
    stop("<qsip_data_object> must be of class qsip_data", call. = FALSE)
  }

  # make sure minimums are not bigger than possible
  if (min_labeled_sources > length(labeled_source_mat_ids)) {
    stop(glue::glue("min_labeled_sources is set to {min_labeled_sources} but labeled_source_mat_ids only has {length(labeled_source_mat_ids)}"))
  }

  if (min_unlabeled_sources > length(unlabeled_source_mat_ids)) {
    stop(glue::glue("min_unlabeled_sources is set to {min_unlabeled_sources} but unlabeled_source_mat_ids only has {length(unlabeled_source_mat_ids)}"))
  }

  # make sure all given source_mat_ids are found in sample_data
  if (length(setdiff(unlabeled_source_mat_ids, qsip_data_object@sample_data@data$source_mat_id) > 0)) {
    stop("Some given unlabeled_source_mat_ids are not found", call. = FALSE)
  } else if (length(setdiff(labeled_source_mat_ids, qsip_data_object@sample_data@data$source_mat_id) > 0)) {
    stop("Some given labeled_source_mat_ids are not found", call. = FALSE)
  }

  # make sure source_mat_ids match expected isotope types
  if (isFALSE(validate_source_isotope(
    qsip_data_object,
    unlabeled_source_mat_ids,
    c("12C", "14N", "16O")
  ))) {
    stop("some of the unlabeled_source_mat_ids have a heavy isotope designation", call. = FALSE)
  }
  if (isFALSE(validate_source_isotope(
    qsip_data_object,
    labeled_source_mat_ids,
    c("13C", "15N", "18O")
  ))) {
    stop("some of the labeled_source_mat_ids have a light isotope designation", call. = FALSE)
  }

  # bind variables
  feature_id <- source_mat_id <- tube_rel_abundance <- fraction_call <- type <- source_call <- labeled <- unlabeled <- sample_id <- WAD <- NULL

  source_mat_ids <- c(unlabeled_source_mat_ids, labeled_source_mat_ids)

  # extract tables
  initial_feature_id_count <- qsip_data_object@tube_rel_abundance |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()

  if (isFALSE(quiet)) {
    message(glue::glue("There are initially {initial_feature_id_count} unique feature_ids"))
  }

  # get long table and subset to only included source_mat_ids
  data <- qsip_data_object@tube_rel_abundance |>
    dplyr::filter(source_mat_id %in% source_mat_ids)

  secondary_feature_id_count <- data |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()

  if (isFALSE(quiet)) {
    message(glue::glue("{secondary_feature_id_count} of these have abundance in at least one fraction of one source_mat_id"))
  }

  if (isFALSE(quiet)) {
    message(rep("=+", 25))
    message("Filtering feature_ids by fraction...")
  }




  by_fraction <- data |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::summarize(
      n_fractions = dplyr::n(),
      tube_rel_abundance = sum(tube_rel_abundance),
      .groups = "drop"
    ) |>
    tidyr::complete(feature_id, # fill in missing fractions with 0
                    source_mat_id,
                    fill = list(
                      n_fractions = 0,
                      tube_rel_abundance = 0
                    )
    ) |>
    dplyr::mutate(type = dplyr::case_when(
      source_mat_id %in% unlabeled_source_mat_ids ~ "unlabeled",
      source_mat_id %in% labeled_source_mat_ids ~ "labeled"
    )) |>
    dplyr::mutate(fraction_call = dplyr::case_when(
      n_fractions == 0 ~ "Zero Fractions",
      type == "unlabeled" & n_fractions < min_unlabeled_fractions ~ "Fraction Filtered",
      type == "labeled" & n_fractions < min_labeled_fractions ~ "Fraction Filtered",
      type == "unlabeled" & n_fractions >= min_unlabeled_fractions ~ "Fraction Passed",
      type == "labeled" & n_fractions >= min_labeled_fractions ~ "Fraction Passed"
    ))

  if (isFALSE(quiet)) {
    fraction_results_message(by_fraction)
  }

  if (isFALSE(quiet)) {
    message(rep("=+", 25))
    message("Filtering feature_ids by source...")
  }

  by_source <- by_fraction |>
    dplyr::filter(fraction_call == "Fraction Passed") |>
    dplyr::group_by(feature_id, type) |>
    dplyr::filter(tube_rel_abundance > 0) |>
    dplyr::summarize(
      n_sources = dplyr::n(),
      mean_tube_rel_abundance = mean(tube_rel_abundance),
      .groups = "drop"
    ) |>
    tidyr::complete(feature_id,
                    type,
                    fill = list(
                      n_sources = 0,
                      tube_rel_abundance = 0
                    )
    ) |>
    dplyr::mutate(source_call = dplyr::case_when(
      n_sources == 0 ~ "Zero Sources",
      type == "unlabeled" & n_sources < min_unlabeled_sources ~ "Source Filtered",
      type == "labeled" & n_sources < min_labeled_sources ~ "Source Filtered",
      type == "unlabeled" & n_sources >= min_unlabeled_sources ~ "Source Passed",
      type == "labeled" & n_sources >= min_labeled_sources ~ "Source Passed"
    ))

  if (isFALSE(quiet)) {
    source_results_message(by_source)
  }

  retained_features <- by_source |>
    dplyr::select(feature_id, type, source_call) |>
    tidyr::pivot_wider(names_from = type, values_from = source_call) |>
    dplyr::filter(labeled == "Source Passed" & unlabeled == "Source Passed") |>
    dplyr::pull(feature_id)

  qsip_data_object@filter_results <- list(
    "group" = group,
    "source_filtered" = by_source,
    "fraction_filtered" = by_fraction,
    "retained_features" = retained_features,
    "labeled_source_mat_ids" = labeled_source_mat_ids,
    "unlabeled_source_mat_ids" = unlabeled_source_mat_ids,
    "min_labeled_sources" = min_labeled_sources,
    "min_unlabeled_sources" = min_unlabeled_sources,
    "min_labeled_fractions" = min_labeled_fractions,
    "min_unlabeled_fractions" = min_unlabeled_fractions
  )

  qsip_data_object@filtered_feature_data <- data |>
    dplyr::filter(feature_id %in% retained_features) |>
    dplyr::select(feature_id, sample_id, tube_rel_abundance) |>
    tidyr::pivot_wider(
      names_from = sample_id,
      values_from = tube_rel_abundance,
      values_fill = 0
    )

  qsip_data_object@filtered_wad_data <- qsip_data_object@wads |>
    dplyr::filter(feature_id %in% retained_features) |>
    dplyr::filter(source_mat_id %in% source_mat_ids) |>
    dplyr::select(feature_id, source_mat_id, WAD) |>
    tidyr::pivot_wider(
      names_from = source_mat_id,
      values_from = WAD
    )

  # update @tube_rel_abundance, @wads, @source_wads and @fraction_counts to only have features/sources that passed the filter

  qsip_data_object@tube_rel_abundance = qsip_data_object@tube_rel_abundance |>
    dplyr::filter(source_mat_id %in% c(qsip_data_object@filter_results$labeled_source_mat_ids, qsip_data_object@filter_results$unlabeled_source_mat_ids)) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features)

  qsip_data_object@wads = qsip_data_object@wads |>
    dplyr::filter(source_mat_id %in% c(qsip_data_object@filter_results$labeled_source_mat_ids, qsip_data_object@filter_results$unlabeled_source_mat_ids)) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features)

  qsip_data_object@source_wads = qsip_data_object@source_wads |>
    dplyr::filter(source_mat_id %in% c(qsip_data_object@filter_results$labeled_source_mat_ids, qsip_data_object@filter_results$unlabeled_source_mat_ids))

  qsip_data_object@fraction_counts = qsip_data_object@fraction_counts |>
    dplyr::filter(source_mat_id %in% c(qsip_data_object@filter_results$labeled_source_mat_ids, qsip_data_object@filter_results$unlabeled_source_mat_ids)) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features)

  return(qsip_data_object)
}




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

  is_qsip_filtered(qsip_data_object, error = TRUE)

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
