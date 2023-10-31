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
#' @param unlabeled_source_mat_ids (*string or strings(s)*) A list of the unlabeled source_mat_ids to filter on
#' @param labeled_source_mat_ids (*string or strings(s)*) A list of the labeled source_mat_ids to filter on
#' @param min_unlabeled_sources (*integer, default: 2*) Minimum number of unlabeled source_mat_ids a feature must be found in.
#' @param min_labeled_sources (*integer, default: 2*) Minimum number of labeled source_mat_ids a feature must be found in.
#' @param min_unlabeled_fractions (*integer, default: 2*) Minimum number of fractions a feature must be found in to be present in an unlabeled source_mat_id
#' @param min_labeled_fractions (*integer, default: 2*) Minimum number of fractions a feature must be found in to be present in an labeled source_mat_id
#'
#' @export
#'
#' @family "qSIP Filtering"
#'
#' @returns An updated `qsip_data` object with a filtered feature dataframe in the
#' `@filtered_feature_data` slot and intermediate data in the `@filter_results`
#' slot for plotting.

run_feature_filter <- function(qsip_data_object,
                               unlabeled_source_mat_ids,
                               labeled_source_mat_ids,
                               min_unlabeled_sources = 2,
                               min_labeled_sources = 2,
                               min_unlabeled_fractions = 2,
                               min_labeled_fractions = 2,
                               quiet = FALSE) {

  # make sure minimums are not bigger than possible
  if (min_labeled_sources > length(labeled_source_mat_ids)) {
    stop(glue::glue("ERROR: min_labeled_sources is set to {min_labeled_sources} but labeled_source_mat_ids only has {length(labeled_source_mat_ids)}"))
  }

  if (min_unlabeled_sources > length(unlabeled_source_mat_ids)) {
    stop(glue::glue("ERROR: min_unlabeled_sources is set to {min_unlabeled_sources} but unlabeled_source_mat_ids only has {length(unlabeled_source_mat_ids)}"))
  }

  # make sure source_mat_ids match expected isotope types
  unlabeled_isotopes = qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% unlabeled_source_mat_ids) |>
    dplyr::pull(isotope) |>
    unique()
  if (length(setdiff(unlabeled_isotopes, c("12C", "14N", "16O"))) > 0) {
    stop("ERROR: some of the unlabeled_source_mat_ids have a heavy isotope designation")
  }

  labeled_isotopes = qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% labeled_source_mat_ids) |>
    dplyr::pull(isotope) |>
    unique()
  if (length(setdiff(labeled_isotopes, c("13C", "15N", "18O"))) > 0) {
    stop("ERROR: some of the labeled_source_mat_ids have a light isotope designation")
  }



  source_mat_ids <- c(unlabeled_source_mat_ids, labeled_source_mat_ids)

  # extract tables

  initial_feature_id_count <- qsip_data_object@tube_rel_abundance |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()

  if (isFALSE(quiet)) {
    message(glue::glue("There are initially {initial_feature_id_count} unique feature_ids"))
  }

  data <- qsip_data_object@tube_rel_abundance |>
    dplyr::filter(source_mat_id %in% source_mat_ids)

  secondary_feature_id_count <- data |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()
  if (isFALSE(quiet)) {
    message(glue::glue("{secondary_feature_id_count} of these have abundance in at least one fraction of one source_mat_id"))
  }

  # make sure all given source_mat_ids are found in sample_data
  if (length(setdiff(unlabeled_source_mat_ids, qsip_data_object@sample_data@data$source_mat_id) > 0)) {
    stop("ERROR: Some given unlabeled_source_mat_ids are not found")
  } else if (length(setdiff(labeled_source_mat_ids, qsip_data_object@sample_data@data$source_mat_id) > 0)) {
    stop("ERROR: Some given labeled_source_mat_ids are not found")
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
    tidyr::complete(feature_id,
      source_mat_id,
      fill = list(
        n_fractions = 0,
        tube_rel_abundance = 0
      )
    ) |> # fill in missing fractions with 0
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
      tube_rel_abundance = sum(tube_rel_abundance),
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

  return(qsip_data_object)
}

#' Filter features by fraction message formatting
#'
#' @param by_fraction by_fraction dataframe from run_feature_filter
#'
#' @export

fraction_results_message <- function(by_fraction) {
  fraction_results <- by_fraction |>
    dplyr::count(feature_id, type, fraction_call) |>
    # unique() |>
    dplyr::count(type, fraction_call) |>
    tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) |>
    tibble::column_to_rownames("fraction_call")

  if (!is.na(fraction_results["Zero Fractions", "unlabeled"])) {
    message(glue::glue_col('{red {fraction_results["Zero Fractions","unlabeled"]}} unlabeled and {red {fraction_results["Zero Fractions","labeled"]}} labeled feature_ids were found in {red zero fractions} in at least one source_mat_id'))
  }

  if (!is.na(fraction_results["Fraction Filtered", "unlabeled"])) {
    message(glue::glue_col('{red {fraction_results["Fraction Filtered","unlabeled"]}} unlabeled and {red {fraction_results["Fraction Filtered","labeled"]}} labeled feature_ids were found in {red too few fractions} in at least one source_mat_id'))
  }

  if (!is.na(fraction_results["Fraction Passed", "unlabeled"])) {
    message(glue::glue_col('{green {fraction_results["Fraction Passed","unlabeled"]}} unlabeled and {green {fraction_results["Fraction Passed","labeled"]}} labeled feature_ids {green passed} the fraction filter'))
  }

  fraction_passed <- by_fraction |>
    dplyr::filter(fraction_call == "Fraction Passed") |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()

  message(glue::glue_col("In total, {green {fraction_passed}} unique feature_ids {green passed} the fraction filtering requirements..."))
}

#' Filter features by source message formatting
#'
#' @param by_source by_source dataframe from run_feature_filter
#'
#' @export

source_results_message <- function(by_source) {
  # fraction_results =

  source_results <- by_source |>
    dplyr::select(feature_id, type, source_call) |>
    unique() |>
    dplyr::count(type, source_call) |>
    tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) |>
    tibble::column_to_rownames("source_call")

  if (!is.na(source_results["Zero Sources", "unlabeled"])) {
    message(glue::glue_col('{red {source_results["Zero Sources","unlabeled"]}} unlabeled and {red {source_results["Zero Sources","labeled"]}} labeled feature_ids failed the source filter because they were found in {red zero sources}'))
  }

  if (!is.na(source_results["Source Filtered", "unlabeled"])) {
    message(glue::glue_col('{red {source_results["Source Filtered","unlabeled"]}} unlabeled and {red {source_results["Source Filtered","labeled"]}} labeled feature_ids failed the source filter because they were found in {red too few sources}'))
  }

  if (!is.na(source_results["Source Passed", "unlabeled"])) {
    message(glue::glue_col('{green {source_results["Source Passed","unlabeled"]}} unlabeled and {green {source_results["Source Passed","labeled"]}} labeled feature_ids {green passed} the source filter'))
  }

  total_passed <- by_source |>
    dplyr::select(feature_id, type, source_call) |>
    tidyr::pivot_wider(names_from = type, values_from = source_call) |>
    dplyr::filter(labeled == "Source Passed" & unlabeled == "Source Passed") |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()

  message(rep("=+", 25))

  message(glue::glue_col("In total, {green {total_passed}} unique feature_ids {green passed} all fraction and source filtering requirements"))
}
