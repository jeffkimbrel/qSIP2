#' Return the feature_ids in a qsip object
#'
#' @param qsip_data_object A `qSIP_data` object that has been run through `run_resampling()`
#' @param filtered (*Boolean*) If TRUE, return the feature_ids from the filtered data
#'
#' @export

get_feature_ids = function(qsip_data_object,
                         filtered = FALSE) {

  is_qsip_data(qsip_data_object, error = TRUE)

  # filtered must be a boolean
  if (!is.logical(filtered)) {
    stop("<filtered> must be TRUE/FALSE", call. = FALSE)
  }

  if (filtered) {

    if (is_qsip_filtered(qsip_data_object, error = FALSE)) {
      return(qsip_data_object@filter_results$retained_features)
    } else {
      stop("No filtered feature_ids in this <qsip_data> object. Try filtering first, or setting the <filtered> to FALSE", call. = FALSE)
    }
  } else {
    return(qsip_data_object@feature_data@data$feature_id)
  }

}


#' Return the source_mat_ids in a qsip object
#'
#' @param qsip_data_object A `qSIP_data` object that has been run through `run_resampling()`
#' @param filtered (*Boolean*) If TRUE, return the feature_ids from the filtered data
#'
#' @export

get_source_mat_ids = function(qsip_data_object,
                           filtered = FALSE) {
  
  is_qsip_data(qsip_data_object, error = TRUE)

  # filtered must be a boolean
  if (!is.logical(filtered)) {
    stop("<filtered> must be TRUE/FALSE", call. = FALSE)
  }

  if (filtered) {

    if (is_qsip_filtered(qsip_data_object, error = FALSE)) {

      source_mat_ids = c(qsip_data_object@filter_results$unlabeled_source_mat_ids,
        qsip_data_object@filter_results$labeled_source_mat_ids)
      
      return(source_mat_ids)
    } else {
      stop("No filtered source_mat_ids in this <qsip_data> object. Try filtering first, or setting the <filtered> to FALSE", call. = FALSE)
    }
  } else {
    return(qsip_data_object@source_data@data$source_mat_id)
  }

}
