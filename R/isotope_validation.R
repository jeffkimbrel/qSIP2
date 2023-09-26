#' Check the validity of an isotope string
#'
#' @param isotope Isotope value or values (string(s))
#' @param isotope_list Isotopes to check against (strings, default: c("12C", "13C", "14N", "15N", "16O", "18O"))
#'
#' @return Returns `NULL` if the isotope strings are valid, or a printed error
#'
#' @export
#' @keywords validation
#'
#' @note The isotope_list may change if isotopolog_label stays a thing. Only the "heavy" isotopes will be allowed.


isotope_validation = function(isotope, isotope_list = c("12C", "13C", "14N", "15N", "16O", "18O")) {

  if (length(setdiff(isotope, isotope_list)) == 0) {
    return(NULL)
  } else {
    for (error in setdiff(isotope, isotope_list)) {
      message(glue::glue(crayon::red("Invalid isotope found: {error}")))

      }
  }
}
