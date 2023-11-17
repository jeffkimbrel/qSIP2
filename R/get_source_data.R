#' Get source dataframe from qSIP2 object
#'
#' This function will return the source data stored in a qSIP2 object as a tibble.
#'
#' @param source_data (*qsip_source_data or qsip_data*) A qSIP2 object with source data
#' @param original_headers (*boolean, default: FALSE*) If TRUE, the original column names will be used
#'
#' @export
#'
#' @returns A tibble with the source data


get_source_data <- function(source_data, original_headers = FALSE) {
  # make sure source_data is the right type
  if ("qsip_data" %in% class(source_data)) {
    sd <- source_data@source_data
  } else if ("qsip_source_data" %in% class(source_data)) {
    sd <- source_data
  } else {
    stop(glue::glue("source_data should be class <qsip_source_data> or <qsip_data>, not {class(source_data)[1]}"))
  }

  # if is not boolean
  if (!is.logical(original_headers)) {
    stop(glue::glue("original_headers should be TRUE/FALSE, not {class(original_headers)[1]}"))
  }

  if (isTRUE(original_headers)) {
    sd@data |>
      dplyr::rename(
        !!(sd@isotope) := isotope,
        !!(sd@isotopolog) := isotopolog,
        !!(sd@source_mat_id) := source_mat_id
      )
  } else {
    sd@data
  }
}
