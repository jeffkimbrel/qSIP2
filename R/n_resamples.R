#' Number of resamples
#'
#' This function returns the number of resamples that were performed on the data.
#' It will return `NULL` if the data has not been resampled.
#'
#' @param qsip_data_object A `qSIP_data` object or list of objects
#'
#' @export

n_resamples <- function(qsip_data_object) {
  if (is_qsip_data(qsip_data_object)) {
    resamples = qsip_data_object@resamples$n
    return(resamples)
  } else if (is_qsip_data_list(qsip_data_object)) {
    resamples = lapply(qsip_data_object, function(x) {x@resamples$n}) |>
      unlist() |>
      tibble::enframe(name = "group", value = "n_resamples")
    return(resamples)
  } else {
    stop("this function requires a <qsip_data> object, or a list of <qsip_data> objects", call. = FALSE)
  }
}
