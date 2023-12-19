#' Check the validity of an isotope string (internal)
#'
#' Often the "bulk" designation is found in this column, so those can optionally
#' be removed from validation checks
#'
#' @param isotope (*string(s)*) Isotope value or values
#' @param isotope_list (*strings, default: c("12C", "13C", "14N", "15N", "16O", "18O")*) Isotopes to check against
#' @param unfractionated_terms (*strings*) Terms to ignore when checking isotope values
#'
#' @returns Returns `NULL` if the isotope strings are valid, or a printed error
#'
#' @export
#'
#' @note The isotope_list may change if isotopolog_label stays a thing. Only the "labeled" isotopes will be allowed.

validate_isotopes <- function(isotope,
                              isotope_list = c("12C", "13C", "14N", "15N", "16O", "18O"),
                              unfractionated_terms = c("bulk", "unfractionated", "T0", "time0", "Time0")) {

  # if any unfractionated terms are found, print a message
  if (any(unfractionated_terms %in% isotope)) {
    for (term in intersect(isotope, unfractionated_terms)) {
      message(glue::glue("Isotope value found that matches typical unfractionated terms: {term}"))
    }
  }

  # remove unfractionated terms from isotope
  isotope = isotope[!isotope %in% unfractionated_terms]

  if (length(setdiff(isotope, isotope_list)) == 0) {
    return(NULL)
  } else {
    for (error in setdiff(isotope, isotope_list)) {
      message(glue::glue("invalid isotope found: {error}"))
    }
    stop("Please fix the isotope names and try again", call. = FALSE)
  }
}
