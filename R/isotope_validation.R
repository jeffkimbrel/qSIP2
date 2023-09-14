#' Check the validity of an isotope string
#'
#' @param isotope A string or vector of isotope strings
#' @param isotope_list A master list of isotopes to check against
#'
#' @return Returns `NULL` if the isotope strings are valid, or a printed error
#'
#' @export
#' @keywords validation

isotope_validation = function(isotope, isotope_list = c("12C", "13C", "14N", "15N", "16O", "18O")) {
  # the isotope_list may change if isotopolog_label stays a thing. Only the "heavy" isotopes will be allowed.

  if (length(setdiff(isotope, isotope_list)) == 0) {
    return(NULL)
  } else {
    for (error in setdiff(isotope, isotope_list)) {
      message(glue::glue(crayon::red("Invalid isotope found: {error}")))

      }
  }
}
