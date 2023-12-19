#' Calculate EAF value (internal)
#'
#' @param M_labeled Molecular weight of the labeled feature
#' @param M Molecular weight of the unlabeled feature
#' @param M_labeledmax Theoretical molecular weight of a completely labeled feature
#' @param isotope The isotope to determine which calculation to run
#'
#' @returns EAF value

calculate_EAF <- function(M_labeled, M, M_labeledmax, isotope) {

  # make sure Ms are numerics
  if (!is.numeric(M)) {
    stop(glue::glue("M should be class <numeric>, not {class(M)[1]}"), call. = FALSE)
  } else if (!is.numeric(M_labeled)) {
    stop(glue::glue("M_labeled should be class <numeric>, not {class(M_labeled)[1]}"), call. = FALSE)
  } else if (!is.numeric(M_labeledmax)) {
    stop(glue::glue("M_labeledmax should be class <numeric>, not {class(M_labeledmax)[1]}"), call. = FALSE)
  }


  validate_isotopes(isotope, isotope_list = c("13C", "15N", "18O"))

  EAF <- (M_labeled - M) / (M_labeledmax - M) * completely_labeled_values(isotope)

  return(EAF)
}
