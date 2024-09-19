#' Return filtering info for a specific feature ID
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been filtered
#' @param feature_id (*string*) A specific feature_id
#'
#' @export

get_filtered_feature_summary = function(qsip_data_object, feature_id) {

  is_qsip_filtered(qsip_data_object, error = TRUE)

  if (!feature_id %in% get_feature_ids(qsip_data_object)) {
    stop(glue::glue("{feature_id} is not a valid feature_id"), call. = FALSE)
  }

  A = qsip_data_object@filter_results$fraction_filtered |>
    dplyr::filter(feature_id == !!feature_id)
  B = qsip_data_object@filter_results$source_filtered |>
    dplyr::filter(feature_id == !!feature_id)
  C = feature_id %in% qsip_data_object@filter_results$retained_features

  return(list("fraction_filter_summary" = A,
              "source_filter_summary" = B,
              "retained" = C
              ))
}
