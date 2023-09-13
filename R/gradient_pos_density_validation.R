#' Check the validity of a density values
#'
#' This is a helper function.
#'
#' @param gradient_pos_density A string or vector of density values
#' @param high A high limit for valid density values
#' @param low A low limit for valid density values
#'
#' @return A TRUE boolean if the density values are valid, or a printed error
#'
#' keywords internal

gradient_pos_density_validation = function(gradient_pos_density, low = 1.55, high = 1.8) {

  if (is.numeric(gradient_pos_density)) {
    if (any(gradient_pos_density > high)) {
      message(glue::glue(crayon::red("Some gradient_pos_density values are higher than {high}")))
    } else if (any(gradient_pos_density < low)) {
      message(glue::glue(crayon::red("Some gradient_pos_density values are lower than {low}")))
    } else {
      return(TRUE)
    }
  } else {
    message(crayon::red("some gradient_pos_density values are non-numeric"))
  }
}
