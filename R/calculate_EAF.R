#' Calculate EAF value
#'
#' @param M_labeled Molecular weight of the labeled feature
#' @param M Molecular weight of the unlabeled feature
#' @param M_labeledmax Theoretical molecular weight of a completely labeled feature
#' @param isotope The isotope to determine which calculation to run

calculate_EAF <- function(M_labeled, M, M_labeledmax, isotope) {
  validate_isotopes(isotope, isotope_list = c("13C", "15N", "18O"))

  EAF <- (M_labeled - M) / (M_labeledmax - M) * completely_labeled_values(isotope)

  return(EAF)
}
