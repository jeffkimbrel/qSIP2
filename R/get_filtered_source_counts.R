#' Return the number of sources the feature is found in
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been filtered
#'
#' @export

get_filtered_source_counts = function(qsip_data_object) {

  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  } else if (length(qsip_data_object@filtered_wad_data) == 0) {
    stop("ERROR: this function requires a qsip object that has been run through run_feature_filter()")
  }

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
