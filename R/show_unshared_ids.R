#' Show source and sample ID overlaps
#'
#' @export

show_unshared_ids <- function(qsip_data_object) {

  shared = qsip_data_object@shared

  message(glue::glue('{length(shared$sources$source_data)} source_mat_id(s) unique to source_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$sources$source_data, collapse = ", "))

  message(glue::glue('\n\n{length(shared$sources$sample_data)} source_mat_id(s) unique to sample_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$sources$sample_data, collapse = ", "))

  message(glue::glue('\n\n{length(shared$samples$sample_data)} sample_id(s) unique to sample_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$samples$sample_data, collapse = ", "))

  message(glue::glue('\n\n{length(shared$samples$feature_id)} sample_id(s) unique to feature_id:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$samples$feature_id, collapse = ", "))
}
