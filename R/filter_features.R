#' Filter features in a qSIP data object
#'
#' Filtering is first done on the fractions, and then on the source_mat_ids. For
#' example, take a feature that is found in three source_mat_ids in 3, 5 and 9
#' fractions. If you set `fraction_counts = 5` and `source_counts = 3` then this
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
#' @param qsip_data (*qsip_data*) An object of `qsip_data` class
#' @param source_mat_ids (*string or strings(s)*) A list of the light and heavy source_mat_ids to filter on
#' @param fraction_counts (*integer*) Minimum number of fractions a feature must be found in to be present in that source_mat_id
#' @param source_counts (*integer*) Minimum number of source_mat_ids a feature must be found in.
#'
#' @export
#'
#' @family "qSIP Filtering"
#'
#' @returns An updated `qsip_data` with a filtered feature dataframe in the
#' `@filtered_feature_data` slot and intermediate data in the `@filter_results`
#' slot for plotting.

filter_features = function(qsip_data,
                           source_mat_ids,
                           source_counts = 1,
                           fraction_counts = 1) {

  # extract tables
  feature_data = qsip_data@tube_rel_abundance
  sample_data = qsip_data@sample_data@data

  # make sure all given source_mat_ids are found in sample_data
  if (length(setdiff(source_mat_ids, sample_data$source_mat_id) > 0)) {
    stop("some given source_mat_ids are not found")
  }

  # get fraction count
  data = feature_data |>
    tidyr::pivot_longer(cols = c(everything(), -feature_id),
                        names_to = "sample_id",
                        values_to = "tube_rel_abundance") |>
    dplyr::left_join(sample_data, by = "sample_id") |>
    dplyr::select(feature_id, sample_id, tube_rel_abundance, source_mat_id, gradient_position) |>
    dplyr::filter(source_mat_id %in% source_mat_ids)

  by_fraction = data |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::mutate(found_in_fraction = dplyr::case_when(
      tube_rel_abundance > 0 ~ 1,
      tube_rel_abundance <= 0 ~ 0
    )) |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::summarize(fraction_count = sum(found_in_fraction),
                     tube_rel_abundance = sum(tube_rel_abundance),
                     .groups = "drop") |>
    dplyr::group_by(source_mat_id, fraction_count) |>
    dplyr::mutate(fraction_keep = dplyr::case_when(
      fraction_count < fraction_counts ~ "Fraction Filtered",
      fraction_count >= fraction_counts ~ "Kept",
      .default = NA
    ))

  by_source = by_fraction |>
    dplyr::ungroup() |>
    dplyr::select(feature_id, source_mat_id, fraction_keep, tube_rel_abundance) |>
    dplyr::group_by(feature_id, fraction_keep) |>
    dplyr::mutate(sources = dplyr::n()) |>
    dplyr::mutate(source_keep = dplyr::case_when(
      sources >= source_counts ~ "Kept",
      sources < source_counts ~ "Source Filtered"
    ))

  retained_features = by_source |>
    dplyr::filter(fraction_keep == "Kept" & source_keep == "Kept") |>
    dplyr::pull(feature_id) |>
    unique()

  qsip_data@filter_results = list("source_filtered" = by_source,
       "fraction_filtered" = by_fraction)

  qsip_data@filtered_feature_data = data |>
    dplyr::filter(feature_id %in% retained_features) |>
    dplyr::select(feature_id, sample_id, tube_rel_abundance) |>
    tidyr::pivot_wider(names_from = sample_id,
                       values_from = tube_rel_abundance,
                       values_fill = 0)

  return(qsip_data)

}






