#' Calculate GC% from density/WAD
#'
#' This function takes a `density` value and an optional `method` and returns the
#' predicted GC% content of a DNA sequence with that density.
#'
#' The `method` parameter changes the formula from that provided by McHugh &
#' Morrissey (`MM`, unpublished) or Schildkraut (`S`, 1962).
#'
#' @param density (*numeric*) Density or WAD values
#' @param method (*string, default: MM*) The GC% calculation method
#'
#' @returns A vector of GC% values
#'
#' @export

calculate_gc_from_density = function(density,
                                     method = "MM") {

  if (method == "MM") {
    (1 / 0.0835059954345993) * (density - 1.64605745338531)
  } else if (method == "S") {
    (1 / 0.098) * (density - 1.66)
  } else {
    stop(glue::glue("ERROR: {method} is not a valid method for GC% calculation. Options are MM (default) or S."))
  }
}


#' Calculate GC% from a sequence
#'
#' This function takes a sequence and counts the number of G and C bases, and divides
#' by the total sequence length.
#'
#' @param sequence (*string*) Density or WAD values
#'
#' @returns A vector of GC% values
#'
#' @export


calculate_gc_from_string = function(sequence) {
  (stringr::str_count(sequence, "g|G") + stringr::str_count(sequence, "c|G")) / stringr::str_length(sequence) * 100
}
