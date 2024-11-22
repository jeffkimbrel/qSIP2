#' Return the number of sources the feature is found in
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been filtered
#'
#' @export

get_filtered_source_counts = function(qsip_data_object) {

  is_qsip_filtered(qsip_data_object, error = TRUE)

  # bind variables
  fraction_call <- feature_id <- type <- counts <- labeled <- unlabeled <- NULL

  qsip_data_object@filter_results$fraction_filtered |>
    dplyr::filter(fraction_call == "Fraction Passed") |>
    dplyr::group_by(feature_id, type) |>
    dplyr::count(name = "counts") |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = type, values_from = counts) |>
    dplyr::rename("unlabeled_sources" = unlabeled,
           "labeled_sources" = labeled)

}
