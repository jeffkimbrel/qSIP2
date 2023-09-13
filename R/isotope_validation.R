isotope_validation = function(isotope, isotope_list = c("12C", "13C", "14N", "15N", "16O", "18O")) {
  # returns TRUE if it passes validation, and FALSE if not.
  # the isotope_list may change if isotopolog_label stays a thing. Only the "heavy" isotopes will be allowed.

  if (length(setdiff(isotope, isotope_list)) == 0) {
    return(TRUE)
  } else {
    glue::glue(crayon::red("Invalid isotope found: {setdiff(isotope, isotope_list)}"))
  }
}
