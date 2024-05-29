#' Check object is qsip_data type
#'
#' @param object The object to check if it is a qsip_data object
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_data = function(object, error = FALSE) {

  # object must be a logical
  if (!is.logical(error)) {
    stop("error must be a logical", call. = FALSE)
  }

  if (!inherits(object, "qsip_data")) {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("qsip_data_object must be a <qsip_data> object, not <{class(object)[1]}>"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}



#' Validate a multi-qsip list object
#'
#' @param qsip_list A list of qsip_data objects
#'
#' @export

is_qsip_data_list <- function(object, error = FALSE) {

  # check if object is a list
  if(!is.list(object)) {
    if (isTRUE(error)) {
      stop("qsip_list must be a list")
    } else {
      return(FALSE)
    }
  }

  is_qsip_data_results = lapply(object, is_qsip_data)

  if (isTRUE(all(unlist(is_qsip_data_results)))) {
    return(TRUE)
  } else {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("qsip_data_object must be a <qsip_data> object, not <{class(object)[1]}>"), call. = FALSE)
    }
  }
}
