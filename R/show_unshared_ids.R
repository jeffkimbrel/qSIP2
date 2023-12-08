#' Show missing source_mat_ids and sample_ids
#'
#' @description
#' This function identifies missing source_mat_ids and sample_ids between the source,
#' sample, and feature data objects.
#'
#' @param qsip_data_object (*qsip_data*) A qSIP data object
#'
#' @export
#'
#' @returns A message with unique IDs per category

show_unshared_ids <- function(qsip_data_object) {

  shared = qsip_data_object@shared

  message(glue::glue('{length(shared$source_mat_ids$source_data)} source_mat_id(s) unique to source_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$source_mat_ids$source_data, collapse = ", "))

  message(glue::glue('\n\n{length(shared$source_mat_ids$sample_data)} source_mat_id(s) unique to sample_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$source_mat_ids$sample_data, collapse = ", "))

  message(glue::glue('\n\n{length(shared$sample_ids$sample_data)} sample_id(s) unique to sample_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$sample_ids$sample_data, collapse = ", "))

  message(glue::glue('\n\n{length(shared$sample_ids$feature_data)} sample_id(s) unique to feature_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$sample_ids$feature_data, collapse = ", "))
}
