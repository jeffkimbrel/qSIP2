#' Return filtering info for a specific feature ID
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been filtered
#' @param feature_id (*string*) A specific feature_id
#'
#' @export

get_filtered_feature_summary = function(qsip_data_object, feature_id) {
  if (!is_qsip_data(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  } else if (length(qsip_data_object@filtered_wad_data) == 0) {
    stop("ERROR: this function requires a qsip object that has been run through run_feature_filter()")
  }

  A = qsip_data_object@filter_results$fraction_filtered |> dplyr::filter(feature_id == feature)
  B = qsip_data_object@filter_results$source_filtered |> dplyr::filter(feature_id == feature)
  C = feature %in% qsip_data_object@filter_results$retained_features

  return(list("fraction_filter_summary" = A,
              "source_filter_summary" = B,
              "retained" = C
              ))
}
