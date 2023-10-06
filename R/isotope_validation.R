#' Check the validity of an isotope string
#'
#' @param isotope (*string(s)*) Isotope value or values
#' @param isotope_list (*strings, default: c("12C", "13C", "14N", "15N", "16O", "18O")*) Isotopes to check against
#'
#' @returns Returns `NULL` if the isotope strings are valid, or a printed error
#'
#' @export
#'
#' @note The isotope_list may change if isotopolog_label stays a thing. Only the "heavy" isotopes will be allowed.

isotope_validation = function(isotope, isotope_list = c("12C", "13C", "14N", "15N", "16O", "18O")) {

  if (length(setdiff(isotope, isotope_list)) == 0) {
    return(NULL)
  } else {
    for (error in setdiff(isotope, isotope_list)) {
      stop(glue::glue(crayon::red("ERROR: Invalid isotope found: {error}")))
    }
    return("Please fix the isotope names and try again")
  }
}
