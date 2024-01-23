#' Return the feature_ids in a qsip object
#'
#' @param qsip_data_object A `qSIP_data` object that has been run through `run_resampling()`
#' @param filtered (*Boolean*) If TRUE, return the feature_ids from the filtered data
#'
#' @export

get_feature_ids = function(qsip_data_object,
                         filtered = FALSE) {
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  }

  if (filtered) {
    feature_ids = qsip_data_object@filter_results$retained_features

    # stop if feature_ids is NULL
    if (is.null(feature_ids)) {
      stop("No filtered feature_ids in this <qsip_data> object. Try filtering first, or setting the <filtered> to FALSE", call. = FALSE)
    } else {
      return(feature_ids)
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
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  }

  if (filtered) {
    source_mat_ids = c(qsip_data_object@filter_results$unlabeled_source_mat_ids,
                    qsip_data_object@filter_results$labeled_source_mat_ids)

    # stop if feature_ids is NULL
    if (is.null(source_mat_ids)) {
      stop("No filtered feature_ids in this <qsip_data> object. Try filtering first, or setting the <filtered> to FALSE", call. = FALSE)
    } else {
      return(source_mat_ids)
    }
  } else {
    return(qsip_data_object@source_data@data$source_mat_id)
  }

}
