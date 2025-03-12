#' Pivot KBase amplicon matrix
#'
#' @param amplicon An amplicon matrix from KBase
#'
#' @keywords internal

pivot_kbase_amplicon_matrix = function(amplicon) {

  # bind variables
  id <- NULL

  amplicon |>
    dplyr::select(-id) |>
    tidyr::pivot_wider(names_from = "column_id",
                       values_from = "value")


}
