#' Get the number of relevant atoms per nucleotide
#'
#' Carbon and nitrogen atoms varies with GC content, but oxygen content is
#' constant for DNA regardless of GC content.
#'
#' @param G (*numeric*) GC percentage
#' @param isotope (*string*) The isotope to use for calculations... either 13C, 15N or 18O

calculate_atoms = function(G, isotope) {

  validate_isotopes(isotope, isotope_list = c("13C", "15N", "18O"))

  if (isotope == "13C") {
    C_atoms <- (-0.5 * G) + 10
    return(C_atoms)
  } else if (isotope == "15N") {
    N_atoms <- (0.5 * G) + 3.5
    return(N_atoms)
  } else if (isotope == "18O") {
    O_atoms <- 6
    return(O_atoms)
  }

}
