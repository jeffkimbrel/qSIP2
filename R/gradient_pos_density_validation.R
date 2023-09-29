#' Check the validity of density values
#'
#' @param gradient_pos_density Density value or values (string(s))
#' @param high A high limit for valid density values (numeric, default: 1.8)
#' @param low A low limit for valid density values (numeric, default: 1.55)
#'
#' @return Returns `NULL` if the density values are valid, or a printed error
#'
#' @export

gradient_pos_density_validation = function(gradient_pos_density, low = 1.55, high = 1.8) {

  if (is.numeric(gradient_pos_density)) {
    if (any(gradient_pos_density > high)) {
      message(glue::glue(crayon::red("Some gradient_pos_density values are higher than {high}")))
    } else if (any(gradient_pos_density < low)) {
      message(glue::glue(crayon::red("Some gradient_pos_density values are lower than {low}")))
    } else {
      return(NULL)
    }
  } else {
    message(crayon::red("some gradient_pos_density values are non-numeric"))
  }
}
