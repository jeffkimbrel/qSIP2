#' Seed used in resampling
#'
#' Returns the seed used in the resampling step, or `NULL` if no specific seed was given.
#'
#' @param qsip_data_object A `qSIP_data` or list of `qSIP_data` objects.
#'
#' @export

resample_seed <- function(qsip_data_object) {
  if (is_qsip_data(qsip_data_object)) {
    seed_used = qsip_data_object@resamples$seed
    return(seed_used)
  } else if (is_qsip_data_list(qsip_data_object)) {
    seed_used = lapply(qsip_data_object, function(x) {x@resamples$seed}) |>
      unlist() |>
      tibble::enframe(name = "group", value = "n_resamples")
    return(seed_used)
  } else {
    stop("this function requires a <qsip_data> object, or a list of <qsip_data> objects", call. = FALSE)
  }
}
