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
      stop(glue::glue("object must be a <qsip_data> object, not <{class(object)[1]}>"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}



#' Validate a multi-qsip list object
#'
#' @param object A list of qsip_data objects
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_data_list <- function(object, error = FALSE) {

  # check if object is a list
  if(!is.list(object)) {
    if (isTRUE(error)) {
      stop("object must be a list")
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
      stop(glue::glue("object must be a <qsip_data> object, not <{class(object)[1]}>"), call. = FALSE)
    }
  }
}




#' Validate a qsip object has been filtered
#'
#' @param object The object to check if it is a filtered qsip_data object
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_filtered <- function(object, error = FALSE) {

  # first check if it is even a qsip object
  is_qsip_data(object, error = TRUE)

  if (length(object@filtered_wad_data) == 0) {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("object is a non-filtered <qsip_data> object"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}





#' Validate a qsip object has been resampled
#'
#' @param object The object to check if it is a resampled qsip_data object
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_resampled <- function(object, error = FALSE) {
  
  # first check if it is even a qsip object
  is_qsip_data(object, error = TRUE)

  if (length(object@resamples) == 0) {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("object is a non-resampled <qsip_data> object"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}



#' Validate a qsip object has been run through growth workflow
#'
#' @param object The object to check if it is a growth qsip_data object
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_growth <- function(object, error = FALSE) {
  
  # first check if it is even a qsip object
  is_qsip_data(object, error = TRUE)

  if (is.null(object@growth$rates))  {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("<object> has not been run through the growth calculations"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}