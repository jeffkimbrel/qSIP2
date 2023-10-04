#' Calculate weighted average density (WAD) values
#'
#' @param tube_rel_abundance (*dataframe*) Output from `calculate_tube_rel_abundance()`
#'
#' @export
#'
#' @returns An list with two objects, 1) a dataframe with WAD info for the feature_ids
#' found in at least one sample, and 2) the fraction counts for all feature_ids,
#' including those not found at all in some samples. It also prints a message to
#' the screen with a count of feature_ids that are entirely missing from at least
#' one sample.
#'

calculate_wads = function(tube_rel_abundance) {

  wads = tube_rel_abundance |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::summarize(WAD = weighted.mean(gradient_pos_density, tube_rel_abundance),
                     n_fractions = dplyr::n(),
                     .groups = "drop") |>
    dplyr::arrange(n_fractions)

  fraction_counts = wads |>
    dplyr::select(-WAD) |>
    tidyr::pivot_wider(names_from = source_mat_id,
                       values_from = n_fractions) |>
    tidyr::pivot_longer(cols = c(everything(), -feature_id),
                        names_to = "source_mat_id",
                        values_to = "n_fractions") |>
    dplyr::arrange(feature_id)

  missing_feature_ids = fraction_counts |>
    dplyr::filter(is.na(n_fractions)) |>
    dplyr::count(feature_id) |>
    dplyr::arrange(n) |>
    dplyr::count(n, name = "counts") |>
    dplyr::summarize(S = sum(counts)) |>
    dplyr::pull(S)

  message(glue::glue("Warning, {missing_feature_ids} feature_ids are have no counts in one or more source_mat_ids"))

  return(list("wads" = wads,
              "fraction_counts" = fraction_counts))

}
