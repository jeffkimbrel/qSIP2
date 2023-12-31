#' Calculate the WAD difference Z (internal)
#'
#' This is just basic subtraction, so probably overkill to make a function!
#'
#' This function corresponds to equation 4 from Hungate, 2015
#'
#' @param labeled (*string*) The column with the labeled WAD or mean labeled WAD value
#' @param unlabeled (*string*) The column with the unlabeled WAD or mean unlabeled WAD value
#'
#' @returns a value for the difference between labeled and unlabeled
#'
#' @export

calculate_Z <- function(labeled, unlabeled) {
  if (any(is.na(c(
    labeled,
    unlabeled
  )))) {
    stop("Can't calculate Z - some WAD values are <NA>")
  }

  stopifnot("Can't calculate Z - some WAD values are not numeric" = is.numeric(c(labeled, unlabeled)))

  Z <- labeled - unlabeled
  return(Z)
}
