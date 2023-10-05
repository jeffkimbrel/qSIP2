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
#' @param source_mat_ids (*string or strings(s)*) A list of the light and heavy source_mat_ids to filter on
#' @param min_fractions (*integer*) Minimum number of fractions a feature must be found in to be present in that source_mat_id
#' @param min_sources (*integer*) Minimum number of source_mat_ids a feature must be found in.
#'
#' @export
#'
#' @family "qSIP Filtering"
#'
#' @returns An updated `qsip_data` object with a filtered feature dataframe in the
#' `@filtered_feature_data` slot and intermediate data in the `@filter_results`
#' slot for plotting.

filter_features = function(qsip_data_object,
                           source_mat_ids,
                           min_sources = 1,
                           min_fractions = 1) {

  # extract tables
  # feature_data = qsip_data_object@tube_rel_abundance
  # sample_data = qsip_data_object@sample_data@data
  data = qsip_data_object@tube_rel_abundance |>
    dplyr::filter(source_mat_id %in% source_mat_ids)

  # make sure all given source_mat_ids are found in sample_data
  # if (length(setdiff(source_mat_ids, sample_data$source_mat_id) > 0)) {
  #   stop("some given source_mat_ids are not found")
  # }

  by_fraction = data |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::mutate(found_in_fraction = dplyr::case_when(
      tube_rel_abundance > 0 ~ 1,
      tube_rel_abundance <= 0 ~ 0
    )) |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::summarize(n_fractions = sum(found_in_fraction),
                     tube_rel_abundance = sum(tube_rel_abundance),
                     .groups = "drop") |>
    dplyr::group_by(source_mat_id, n_fractions) |>
    dplyr::mutate(fraction_keep = dplyr::case_when(
      n_fractions < min_fractions ~ "Fraction Filtered",
      n_fractions >= min_fractions ~ "Kept",
      .default = NA
    ))

  by_source = by_fraction |>
    dplyr::ungroup() |>
    dplyr::select(feature_id, source_mat_id, fraction_keep, tube_rel_abundance) |>
    dplyr::group_by(feature_id, fraction_keep) |>
    dplyr::mutate(sources = dplyr::n()) |>
    dplyr::mutate(source_keep = dplyr::case_when(
      sources >= min_sources ~ "Kept",
      sources < min_sources ~ "Source Filtered"
    ))

  retained_features = by_source |>
    dplyr::filter(fraction_keep == "Kept" & source_keep == "Kept") |>
    dplyr::pull(feature_id) |>
    unique()

  qsip_data_object@filter_results = list("source_filtered" = by_source,
       "fraction_filtered" = by_fraction,
       "source_mat_ids" = source_mat_ids,
       "min_sources" = min_sources,
       "min_fractions" = min_fractions,
       "retained_features" = retained_features)

  qsip_data_object@filtered_feature_data = data |>
    dplyr::filter(feature_id %in% retained_features) |>
    dplyr::select(feature_id, sample_id, tube_rel_abundance) |>
    tidyr::pivot_wider(names_from = sample_id,
                       values_from = tube_rel_abundance,
                       values_fill = 0)

  return(qsip_data_object)

}






