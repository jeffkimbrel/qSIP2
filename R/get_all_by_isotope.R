#' Get source_mat_ids meeting certain isotope conditions
#'
#' @param qsip_data_object (*qsip_data or qsip_source_data*) A qsip object with source data
#' @param isotopes (*string(s)*) Isotopes used to pull source_mat_ids. Can be a standard isotope name (e.g. `12C`) or special terms `labeled` or `unlabeled`
#'
#' @returns A vector of source_mat_ids. It may also print some messages.
#'
#' @export

get_all_by_isotope <- function(qsip_data_object, isotopes) {
  if ("qsip_data" %in% class(qsip_data_object)) {
    source_data <- qsip_data_object@source_data@data
  } else if ("qsip_source_data" %in% class(qsip_data_object)) {
    source_data <- qsip_data_object@data
  } else {
    stop("qsip_data_object must be class <qsip_data> or <qsip_source_data>")
  }

  if ("labeled" %in% isotopes) {
    isotopes <- c("13C", "15N", "18O")
  } else if ("unlabeled" %in% isotopes) {
    isotopes <- c("12C", "14N", "16O")
  }

  # verify given isotopes are valid
  validate_isotopes(isotopes)

  # filter for isotopes
  source_mat_ids <- source_data |>
    dplyr::filter(isotope %in% isotopes) |>
    dplyr::select(source_mat_id, isotope)

  # error if no source_mat_ids are found that match the criteria
  if (nrow(source_mat_ids) == 0) {
    i <- paste(isotopes, collapse = ", ")
    stop(glue::glue_col("No source_mat_ids found with isotopes {red {i}}"))
  }

  # print a message for each isotope that didn't have any hits. This is FYI and doesn't stop the function
  for (isotope in isotopes) {
    if (!isotope %in% source_mat_ids$isotope) {
      message(glue::glue("WARNING: {isotope} not found in data"))
    }
  }

  # return a list of source_mat_ids
  return(unique(source_mat_ids$source_mat_id))
}
