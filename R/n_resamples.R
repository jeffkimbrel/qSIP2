#' Number of resamples
#'
#' @param qsip_data_object A `qSIP_data` object that has been run through `run_resampling()`
#'
#' @export

n_resamples <- function(qsip_data_object) {
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  } else if (length(qsip_data_object@resamples) == 0) {
    stop("this function requires a qsip object that has been run through run_resampling()", call. = FALSE)
  }

  qsip_data_object@resamples$n
}
