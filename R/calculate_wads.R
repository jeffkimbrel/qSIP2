#' Calculate weighted average density (WAD) values
#'
#' @param tube_rel_abundance (*dataframe*) Output from `calculate_tube_rel_abundance()`
#'
#' @export
#'
#' @returns A list with two objects, 1) a dataframe with WAD info for the feature_ids
#' found in at least one sample, and 2) the fraction counts for all feature_ids,
#' including those not found at all in some samples. It also prints a message to
#' the screen with a count of feature_ids that are entirely missing from at least
#' one sample.
#'

calculate_wads <- function(tube_rel_abundance) {
  wads <- tube_rel_abundance |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::summarize(
      WAD = weighted.mean(gradient_pos_density, tube_rel_abundance),
      n_fractions = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(n_fractions)

  fraction_counts <- wads |>
    dplyr::select(-WAD) |>
    tidyr::pivot_wider(
      names_from = source_mat_id,
      values_from = n_fractions
    ) |>
    tidyr::pivot_longer(
      cols = c(everything(), -feature_id),
      names_to = "source_mat_id",
      values_to = "n_fractions"
    ) |>
    dplyr::arrange(feature_id)

  missing_feature_ids <- fraction_counts |>
    dplyr::filter(is.na(n_fractions)) |>
    dplyr::count(feature_id) |>
    dplyr::arrange(n) |>
    dplyr::count(n, name = "counts") |>
    dplyr::summarize(S = sum(counts)) |>
    dplyr::pull(S)

  message(glue::glue_col("WARNING: {red {missing_feature_ids}} feature_ids have no counts in one or more source_mat_ids"))

  return(list(
    "wads" = wads,
    "fraction_counts" = fraction_counts
  ))
}


#' Calculate global weighted average density (WAD) value for a source_mat_id
#'
#' @param sample_data (*qsip_sample_data*) Sample data object
#'
#' @export
#'
#' @returns A dataframe with two columns, 1) the source_mat_id and 2) the global
#' WAD value for that source_mat_id

calculate_source_wads <- function(sample_data) {
  sample_data@data |>
    dplyr::group_by(source_mat_id) |>
    dplyr::summarize(
      WAD = weighted.mean(gradient_pos_density, gradient_pos_rel_amt),
      .groups = "drop"
    )
}
