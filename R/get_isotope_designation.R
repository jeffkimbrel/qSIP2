#' Infer which isotope calculations to use (internal)
#'
#' This uses the list of the source_mat_ids in a filtered qsip_data object to get
#' their isotope designation for two reasons, 1) to make sure they are comparable
#' (e.g. are all 12C/13C, 14N/15N or 16O/18O with no mismatches), and 2) to return
#' the best guess of the labeled isotope so EAF calculations will proceed correctly.
#'
#' @param qsip_data_object (*qsip_data*) A `qsip_data` object
#'
#' @returns A single labeled isotope designation of 13C, 15N or 18O, and gives an
#' error if an inference cannot be made.


get_isotope_designation <- function(qsip_data_object) {

  stopifnot("ERROR: qsip_data_object must be of class <qsip_data>" = "qsip_data" %in% class(qsip_data_object))

  source_mat_ids_to_verify <- c(
    qsip_data_object@filter_results$labeled_source_mat_ids,
    qsip_data_object@filter_results$unlabeled_source_mat_ids
  )

  isotopes <- qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% source_mat_ids_to_verify) |>
    dplyr::pull(isotope) |>
    unique()

  # make sure isotopes contains only valid options (15N and 18O are not covered with testthat)
  validate_isotopes(isotopes)

  if (length(setdiff(isotopes, c("12C", "13C"))) == 0) {
    return("13C")
  } else if (length(setdiff(isotopes, c("14N", "15N"))) == 0) {
    return("15N")
  } else if (length(setdiff(isotopes, c("16O", "18O"))) == 0) {
    return("18O")
  } else {
    stop("something went wrong with inferring which isotope calculation to use")
  }
}
