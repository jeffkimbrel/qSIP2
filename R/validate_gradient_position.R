#' Check the validity of gradient position values (internal)
#'
#' Valid gradient positions are integers. The value can be `-1` to represent
#' "bulk" or non-qSIP samples.
#'
#' @param gradient_position (*string or strings*) Gradient position value or values
#'
#' @returns `NULL` if the gradient position values are valid, or a
#' printed error
#'
#' @export


validate_gradient_position <- function(gradient_position) {
  if (is.numeric(gradient_position)) {
    # https://www.tutorialspoint.com/how-to-check-if-all-values-in-a-vector-are-integer-or-not-in-r
    if (all(gradient_position - floor(gradient_position) == 0)) {
      return(NULL)
    } else {
      stop("some gradient_position values are not integers", call. = FALSE)
    }
  } else {
    stop("some gradient_position values are non-numeric", call. = FALSE)
  }
}
