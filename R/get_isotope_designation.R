#' Infer which isotope calculations to use (internal)
#'
#' This uses the list of the source_mat_ids in a filtered qsip_data object to get
#' their isotope designation for two reasons, 1) to make sure they are comparable
#' (e.g. are all 12C/13C, 14N/15N or 16O/18O with no mismatches), and 2) to return
#' the best guess of the labeled isotope so EAF calculations will proceed correctly.
#'
#' As of v0.15.2 it is now possible to have a mismatch. This is particularly
#' important with multiple isotope studies where you may want to compare a 13C sample
#' to a 12C sample, but also an 18O against the same 12C sample.
#'
#' @param qsip_data_object (*qsip_data*) A `qsip_data` object
#' @param unlabeled_source_mat_ids (*character*) A vector of source_mat_ids that are unlabeled
#' @param labeled_source_mat_ids (*character*) A vector of source_mat_ids that are labeled
#'
#' @returns A single labeled isotope designation of 13C, 15N or 18O, and gives an
#' error if an inference cannot be made.


get_isotope_designation <- function(qsip_data_object, unlabeled_source_mat_ids, labeled_source_mat_ids) {

  # check unlabeled_source_mat_ids is a character vector with a size of 1 or more
  if (!is.character(unlabeled_source_mat_ids) | length(unlabeled_source_mat_ids) == 0) {
    stop("unlabeled_source_mat_ids must be a character vector with a size of 1 or more")
  }

  # check labeled_source_mat_ids is a character vector with a size of 1 or more
  if (!is.character(labeled_source_mat_ids) | length(labeled_source_mat_ids) == 0) {
    stop("labeled_source_mat_ids must be a character vector with a size of 1 or more")
  }

  is_qsip_data(qsip_data_object, error = TRUE)

  unlabeled_isotopes = qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% unlabeled_source_mat_ids) |>
    dplyr::pull(isotope) |>
    unique()
  validate_isotopes(unlabeled_isotopes, c("12C", "14N", "16O"))

  labeled_isotopes <- qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% labeled_source_mat_ids) |>
    dplyr::pull(isotope) |>
    unique()
  validate_isotopes(labeled_isotopes, c("13C", "15N", "18O"))

  if (length(unique(labeled_isotopes)) == 1) {
    return(unique(labeled_isotopes))
  } else {
    stop("There is a mixture of multiple labeled isotopes in this group, which is not allowed")
  }
}
