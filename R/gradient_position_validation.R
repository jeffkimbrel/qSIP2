#' Check the validity of gradient position values
#'
#' Valid gradient positions are integers. The value can be `-1` to represent "bulk" or non-qSIP samples.
#'
#' @param gradient_position A string or vector of gradient position values
#'
#' @return Returns `NULL` if the gradient position values are valid, or a printed error
#'
#' @export

gradient_position_validation = function(gradient_position) {
  if (is.numeric(gradient_position)) {
  # https://www.tutorialspoint.com/how-to-check-if-all-values-in-a-vector-are-integer-or-not-in-r
    if (all(gradient_position-floor(gradient_position)==0)) {
      return(NULL)
    } else {
      message(crayon::red("some gradient_position values are not integers"))
    }
  } else {
    message(crayon::red("some gradient_position values are non-numeric"))
  }
}
