#' Show missing source_mat_ids and sample_ids
#'
#' @description
#'
#' This function identifies shared and missing source_mat_ids and sample_ids between the source,
#' sample, and feature data objects.
#'
#' @param qsip_data_object (*qsip_data*) A qSIP data object
#'
#' @export
#'
#' @returns A message with unique IDs per category

show_unshared_ids <- function(qsip_data_object) {

  is_qsip_data(qsip_data_object, error = TRUE)

  return(qsip_data_object@shared)
}
