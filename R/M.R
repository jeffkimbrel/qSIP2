#' Calculate molecular weight of the labeled feature
#'
#' The GC value given to this equation is usually calculated from the density
#' value, not derived from the sequence itself.
#'
#' This function corresponds to equation 6 from Hungate, 2015.
#'
#' @param G (*numeric*) GC content of a feature, ranges from 0-1.
#'
#' @returns `M` is the molecular weight of a sequence with `G` GC content
#'
#' @export

calculate_M <- function(G) {

  if (!is.numeric(G)) {
    stop(glue::glue("ERROR: G should be numeric and not {class(G)}"))
  }

  M <- (G * 0.496) + 307.691
  M
}



#' Calculate molecular weight of the labeled feature
#'
#' This function corresponds to equation 7 from Hungate, 2015
#'
#' @param M (*numeric*) Molecular weight of the unlabeled feature
#' @param atom_count (*numeric*) The count of the relevant atoms (C, N or O)
#' @param isotope (*string*) The heavy isotope determining which calculation to run. Needs to be 13C, 15N or 18O
#' @param propO (*numeric*) Proportion of oxygen atoms in DNA that come from environmental water
#'
#' @returns `M_labeledmax` is the theoretical maximum molecular weight the labeled feature could be
#'
#' @export

calculate_M_labeledmax <- function(M,
                                   atom_count,
                                   isotope,
                                   propO = 1) {

  validate_isotopes(isotope, isotope_list = c("13C", "15N", "18O"))

  if (propO > 1 | propO < 0) {
    stop("ERROR: prop0 should be between 0 and 1")
  }

  if (!is.numeric(M)) {
    stop(glue::glue("ERROR: M should be numeric, not {class(M)}"))
  }

  if (isotope == "13C") {
    # assumes unlabeled DNA already contains a minute amount of 13C (at the
    # natural abundance level of VPDB)
    M_labeledmax <- M + (atom_count * (1.008665 * (1000000 / (1000000 + 11237.2))))
    return(M_labeledmax)

  } else if (isotope == "15N") {
    # assumes unlabeled DNA already contains minute amounts of 15N (at the
    # natural abundance level of AIR-N2)
    M_labeledmax <- M + (atom_count * (1.008665 * (1000000 / (1000000 + (1000000 / 272)))))
    return(M_labeledmax)

  } else if (isotope == "18O") {
    # assumes unlabeled DNA already contains minute amounts of 18O and 17O
    # (at the natural abundance levels of those isotopes in VSMOW)
    M_labeledmax <- M + (atom_count * propO * ((1.008665 * 2 * (1000000 / (1000000 + 379.9 + 2005.20))) + (1.008665 * 1 * (379.9 / (1000000 + 379.9 + 2005.20)))))
    return(M_labeledmax)
  }
}




#' Calculate molecular weight of the labeled feature
#'
#' @param M (*numeric*) Molecular weight of the unlabeled feature
#' @param W_lab_mean (*numeric*) WAD (or mean WAD) value of the labeled feature
#' @param W_unlab_mean (*numeric*) WAD (or mean WAD) value of the unlabeled feature
#'
#' @returns `M_labeled` is the molecular weight of the labeled feature
#'
#' @export

calculate_M_labeled <- function(M, W_lab_mean, W_unlab_mean) {

  if (any(!is.numeric(c(M, W_lab_mean, W_unlab_mean)))) {
    stop("ERROR: some values are non-numeric")
  }

  M_labeled <- M * (W_lab_mean / W_unlab_mean)
  return(M_labeled)
}
