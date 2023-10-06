#' Get source_mat_ids meeting certain isotope conditions
#'
#' @param qsip_data_object (*qsip_data or qsip_source_data*) A qsip object with source data
#' @param isotopes (*string(s)*) Isotopes used to pull source_mat_ids. Can be a standard isotope name (i.e. "12C") or "labeled"/"unlabeled"
#'
#' @returns A vector of source_mat_ids. It may also print some messages.
#'
#' @export

get_all_by_isotope = function(qsip_data_object, isotopes) {

  if ("qsip_data" %in% class(qsip_data_object)) {
    source_data = qsip_data_object@source_data@data
  } else if ("qsip_source_data" %in% class(qsip_data_object)) {
    source_data = qsip_data_object@data
  } else {
    stop("qsip_data_object must be of type qsip_data or qsip_source_data")
  }

  if ("labeled" %in% isotopes) {
    isotopes = c("13C", "15N", "18O")
  } else if ("unlabeled" %in% isotopes) {
    isotopes = c("12C", "14N", "16O")
  }

  qSIP2::isotope_validation(isotopes)

  source_mat_ids = source_data |>
    dplyr::filter(isotope %in% isotopes) |>
    dplyr::select(source_mat_id, isotope)

  if (nrow(source_mat_ids) == 0) {
    i = paste(isotopes, collapse = ", ")
    stop(glue::glue_col("ERROR: No source_mat_ids found with isotopes {red {i}}"))
  }

  for (isotope in isotopes) {
    if (!isotope %in%  source_mat_ids$isotope) {
      message(glue::glue("WARNING: {isotope} not found in data"))
    }
  }

  return(unique(source_mat_ids$source_mat_id))

}
