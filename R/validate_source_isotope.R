#' Validate the given source mat ids have the expected labeled/unlabeled designation (internal)
#'
#' Currently used by run_feature_filter() to make sure user given labeled or unlabeled
#' source_mat_ids are not incorrect with respective to their source_data.
#'
#' @param qsip_data_object A qsip_data object
#' @param source_mat_ids A character vector of source_mat_ids
#' @param isotope_list A character vector of isotopes to check against
#'
#' @export
#'
#' @returns TRUE (all match) or FALSE (some don't match)

validate_source_isotope <- function(qsip_data_object, source_mat_ids, isotope_list) {
  source_mat_id <- isotope <- NULL

  source_isotopes <- qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% source_mat_ids) |>
    dplyr::pull(isotope) |>
    unique()

  if (length(setdiff(source_isotopes, isotope_list)) > 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
