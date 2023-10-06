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

filter_features = function(qsip_data_object,
                           unlabeled_source_mat_ids,
                           labeled_source_mat_ids,
                           min_unlabeled_sources = 2,
                           min_labeled_sources = 2,
                           min_unlabeled_fractions = 2,
                           min_labeled_fractions = 2) {

  source_mat_ids = c(unlabeled_source_mat_ids, labeled_source_mat_ids)

  # extract tables
  data = qsip_data_object@tube_rel_abundance |>
    dplyr::filter(source_mat_id %in% source_mat_ids)

  # make sure all given source_mat_ids are found in sample_data
  if (length(setdiff(unlabeled_source_mat_ids, qsip_data_object@sample_data@data$source_mat_id) > 0)) {
    stop("ERROR: Some given unlabeled_source_mat_ids are not found")
  } else if (length(setdiff(labeled_source_mat_ids, qsip_data_object@sample_data@data$source_mat_id) > 0)) {
    stop("ERROR: Some given labeled_source_mat_ids are not found")
  }

  message(rep("=+", 25))
  message("Filtering feature_ids by fraction...")

  by_fraction = data |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::summarize(n_fractions = dplyr::n(),
                     tube_rel_abundance = sum(tube_rel_abundance),
                     .groups = "drop") |>
    tidyr::complete(feature_id,
                    source_mat_id,
                    fill = list(n_fractions = 0,
                                tube_rel_abundance = 0)) |> # fill in missing fractions with 0
    dplyr::mutate(type = dplyr::case_when(
      source_mat_id %in% unlabeled_source_mat_ids ~ "unlabeled",
      source_mat_id %in% labeled_source_mat_ids ~ "labeled")
    ) |>
    dplyr::mutate(fraction_call = dplyr::case_when(
      n_fractions == 0 ~ "Zero Fractions",
      type == "unlabeled" & n_fractions < min_unlabeled_fractions ~ "Fraction Filtered",
      type == "labeled" & n_fractions < min_labeled_fractions ~ "Fraction Filtered",
      type == "unlabeled" & n_fractions >= min_unlabeled_fractions ~ "Fraction Passed",
      type == "labeled" & n_fractions >= min_labeled_fractions ~ "Fraction Passed"
    ))

  fraction_results_message(by_fraction)


  message(rep("=+", 25))
  message("Filtering feature_ids by source")

  by_source = by_fraction |>
    dplyr::filter(fraction_call == "Fraction Passed") |>
    dplyr::group_by(feature_id, type) |>
    dplyr::filter(tube_rel_abundance > 0) |>
    dplyr::summarize(n_sources = dplyr::n(),
                     tube_rel_abundance = sum(tube_rel_abundance),
                     .groups = "drop") |>
    tidyr::complete(feature_id,
                    type,
                    fill = list(n_sources = 0,
                                tube_rel_abundance = 0)) |>
    dplyr::mutate(source_call = dplyr::case_when(
      n_sources == 0 ~ "Zero Sources",
      type == "unlabeled" & n_sources < min_unlabeled_sources ~ "Source Filtered",
      type == "labeled" & n_sources < min_labeled_sources ~ "Source Filtered",
      type == "unlabeled" & n_sources >= min_unlabeled_sources ~ "Source Passed",
      type == "labeled" & n_sources >= min_labeled_sources ~ "Source Passed"
    ))

  source_results_message(by_source)


  retained_features = by_source |>
    dplyr::select(feature_id, type, source_call) |>
    tidyr::pivot_wider(names_from = type, values_from = source_call) |>
    dplyr::filter(labeled == "Source Passed" & unlabeled == "Source Passed") |>
    dplyr::pull(feature_id)

  qsip_data_object@filter_results = list("source_filtered" = by_source,
                                         "fraction_filtered" = by_fraction,
                                         "retained_features" = retained_features,
                                         "unlabeled_source_mat_ids" = unlabeled_source_mat_ids,
                                         "min_unlabeled_sources" = min_unlabeled_sources,
                                         "min_unlabeled_fractions" = min_unlabeled_fractions,
                                         "unlabeled_source_mat_ids" = unlabeled_source_mat_ids,
                                         "min_unlabeled_sources" = min_unlabeled_sources,
                                         "min_unlabeled_fractions" = min_unlabeled_fractions
  )

  qsip_data_object@filtered_feature_data = data |>
    dplyr::filter(feature_id %in% retained_features) |>
    dplyr::select(feature_id, sample_id, tube_rel_abundance) |>
    tidyr::pivot_wider(names_from = sample_id,
                       values_from = tube_rel_abundance,
                       values_fill = 0)

  qsip_data_object@filtered_wad_data = qsip_data_object@wads |>
    dplyr::filter(feature_id %in% retained_features) |>
    dplyr::filter(source_mat_id %in% source_mat_ids) |>
    dplyr::select(feature_id, source_mat_id, WAD) |>
    tidyr::pivot_wider(names_from = source_mat_id,
                       values_from = WAD)

  return(qsip_data_object)

}

#' Filter features by fraction message formatting
#' @param by_fraction by_fraction dataframe from filter_features
#'
#' @export

fraction_results_message = function(by_fraction) {
  fraction_results = by_fraction |>
    dplyr::select(feature_id, type, fraction_call) |>
    unique() |>
    dplyr::count(type, fraction_call) |>
    tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) |>
    tibble::column_to_rownames("fraction_call")

  if (!is.na(fraction_results["Zero Fractions","unlabeled"])) {
    message(glue::glue_col('{red {fraction_results["Zero Fractions","unlabeled"]}} unlabeled and {red {fraction_results["Zero Fractions","labeled"]}} labeled feature_ids {red failed} the fraction filter because they were found in zero fractions'))
  }

  if (!is.na(fraction_results["Fraction Filtered","unlabeled"])) {
    message(glue::glue_col('{red {fraction_results["Fraction Filtered","unlabeled"]}} unlabeled and {red {fraction_results["Fraction Filtered","labeled"]}} labeled feature_ids {red failed} the fraction filter because they were found in too few fractions'))
  }

  if (!is.na(fraction_results["Fraction Passed","unlabeled"])) {
    message(glue::glue_col('{green {fraction_results["Fraction Passed","unlabeled"]}} unlabeled and {green {fraction_results["Fraction Passed","labeled"]}} labeled feature_ids {green passed} the fraction filter'))
  }

  fraction_passed = by_fraction |>
    dplyr::filter(fraction_call == "Fraction Passed") |>
    dplyr::pull(feature_id) |> unique() |> length()

  message(glue::glue_col("In total, {green {fraction_passed}} feature_ids {green passed} the fraction filtering requirements..."))
}

#' Filter features by source message formatting
#' @param by_source by_source dataframe from filter_features
#'
#' @export

source_results_message = function(by_source) {
  #fraction_results =

  source_results = by_source |>
    dplyr::select(feature_id, type, source_call) |>
    unique() |>
    dplyr::count(type, source_call) |>
    tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) |>
    tibble::column_to_rownames("source_call")

  if (!is.na(source_results["Zero Sources","unlabeled"])) {
    message(glue::glue_col('{red {source_results["Zero Sources","unlabeled"]}} unlabeled and {red {source_results["Zero Sources","labeled"]}} labeled feature_ids {red failed} the source filter because they were found in zero sources'))
  }

  if (!is.na(source_results["Source Filtered","unlabeled"])) {
    message(glue::glue_col('{red {source_results["Source Filtered","unlabeled"]}} unlabeled and {red {source_results["Source Filtered","labeled"]}} labeled feature_ids {red failed} the source filter because they were found in too few sources'))
  }

  if (!is.na(source_results["Source Passed","unlabeled"])) {
    message(glue::glue_col('{green {source_results["Source Passed","unlabeled"]}} unlabeled and {green {source_results["Source Passed","labeled"]}} labeled feature_ids {green passed} the source filter'))
  }

  total_passed = by_source |>
    dplyr::select(feature_id, type, source_call) |>
    tidyr::pivot_wider(names_from = type, values_from = source_call) |>
    dplyr::filter(labeled == "Source Passed" & unlabeled == "Source Passed") |>
    dplyr::pull(feature_id) |>
    unique() |> length()

  message(rep("=+", 25))

  message(glue::glue_col("In total, {green {total_passed}} feature_ids {green passed} all filtering requirements"))
}




