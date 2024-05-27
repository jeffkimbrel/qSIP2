#' Seed used in resampling
#'
#' Returns the seed used in the resampling step, or `NA` if no specific seed was given.
#'
#' @param qsip_data_object A `qSIP_data` object that has been run through `run_resampling()`
#'
#' @export

resample_seed <- function(qsip_data_object) {
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  } else if (length(qsip_data_object@resamples) == 0) {
    stop("this function requires a qsip object that has been run through run_resampling()", call. = FALSE)
  }

  if (is.null(qsip_data_object@resamples$seed)) {
    return(NA)
  } else {
    qsip_data_object@resamples$seed
  }
}
