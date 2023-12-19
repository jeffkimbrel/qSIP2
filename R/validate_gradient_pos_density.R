#' Check the validity of density values (internal)
#'
#' @param df (*dataframe*) A two-column dataframe with density positions and density values
#' @param high (*numeric, default: 1.8*) A high limit for valid density values
#' @param low (*numeric, default: 1.55*) A low limit for valid density values
#'
#' @returns Returns `NULL` if the density values are valid, or a printed error
#'
#' @export

validate_gradient_pos_density <- function(df, low = 1.55, high = 1.8) {

  stopifnot("data should be class <data.frame>" = "data.frame" %in% class(df))

  # message if any fractions are -1 indicating bulk data. These will be ignored
  # from the validation because there densities are often NA
  if(any(df$gradient_position == -1)) {
    #message("some samples have a gradient_position of -1 and their gradient_pos_density will not be validated")
    gradient_pos_density = df |>
      dplyr::filter(gradient_position > 0) |>
      dplyr::pull(gradient_pos_density)
  } else{
    gradient_pos_density = df |>
      dplyr::pull(gradient_pos_density)
  }

  stopifnot("some gradient_pos_density values are non-numeric" = is.numeric(gradient_pos_density))

  if (any(gradient_pos_density > high)) {
    stop(glue::glue("some gradient_pos_density values are higher than {high}"))
  } else if (any(gradient_pos_density < low)) {
    stop(glue::glue("some gradient_pos_density values are lower than {low}"))
  } else {
    return(NULL)
  }

}
