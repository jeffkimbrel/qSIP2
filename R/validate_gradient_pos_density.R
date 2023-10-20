#' Check the validity of density values
#'
#' @param gradient_pos_density (*string or strings*) Density value or values
#' @param high (*numeric, default: 1.8*) A high limit for valid density values
#' @param low (*numeric, default: 1.55*) A low limit for valid density values
#'
#' @return Returns `NULL` if the density values are valid, or a printed error
#'
#' @export

validate_gradient_pos_density <- function(gradient_pos_density, low = 1.55, high = 1.8) {
  if (is.numeric(gradient_pos_density)) {
    if (any(gradient_pos_density > high)) {
      stop(glue::glue(crayon::red("ERROR: Some gradient_pos_density values are higher than {high}")))
    } else if (any(gradient_pos_density < low)) {
      stop(glue::glue(crayon::red("ERROR: Some gradient_pos_density values are lower than {low}")))
    } else {
      return(NULL)
    }
  } else {
    stop(crayon::red("ERROR: some gradient_pos_density values are non-numeric"))
  }
}
