#' Returns the max labeling of a given isotope
#'
#' @param isotope The heavy isotope (13C, 15N or 18O)
#'
#' @export

completely_labeled_values <- function(isotope) {

  isotope_validation(isotope, isotope_list = c("13C", "15N", "18O"))

  if (isotope == "13C") {
    return(1-(11237.2/(1000000+11237.2)))
  } else if (isotope == "15N") {
    return(1-((1000000/272)/(1000000+(1000000/272))))
  } else if (isotope == "18O") {
    return(fully_labeled = 1-(2005.20/(1000000+379.9+2005.20)))
  }
}
