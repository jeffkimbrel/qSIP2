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
#'
#' @returns A single labeled isotope designation of 13C, 15N or 18O, and gives an
#' error if an inference cannot be made.


get_isotope_designation <- function(qsip_data_object) {

  stopifnot("ERROR: qsip_data_object must be of class <qsip_data>" = "qsip_data" %in% class(qsip_data_object))

  unlabeled_isotopes = qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% qsip_data_object@filter_results$unlabeled_source_mat_ids) |>
    dplyr::pull(isotope) |>
    unique()
  if (!is.null(validate_isotopes(unlabeled_isotopes, c("12C", "14N", "16O")))) {
    stop("One of the unlabeled isotope designations is not 12C, 14N or 16O")
  }


  labeled_isotopes <- qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% qsip_data_object@filter_results$labeled_source_mat_ids,) |>
    dplyr::pull(isotope) |>
    unique()
  if (!is.null(validate_isotopes(labeled_isotopes, c("13C", "15N", "18O")))) {
    stop("One of the labeled isotope designations is not 13C, 15N or 18O")
  }

  if (length(unique(labeled_isotopes)) == 1) {
    return(unique(labeled_isotopes))
  } else {
    stop("There is a mixture of multiple labeled isotopes in this group, which is not allowed")
  }
}
