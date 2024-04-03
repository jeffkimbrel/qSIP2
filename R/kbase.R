#' Pivot KBase amplicon matrix
#'
#' @param amplicon An amplicon matrix from KBase
#'
#' @export


pivot_kbase_amplicon_matrix = function(amplicon) {
  amplicon |>
    tidyr::pivot_wider(names_from = "column_id",
                       values_from = "value")


}
