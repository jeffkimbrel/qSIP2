#' Get sample dataframe from qSIP2 object
#'
#' This function will return the sample data stored in a qSIP2 object as a tibble.
#'
#' @param sample_data (*qsip_sample_data or qsip_data*) A qSIP2 object with sample data
#' @param original_headers (*boolean, default: FALSE*) If TRUE, the original column names will be used
#'
#' @export
#'
#' @returns A tibble with the sample data


get_sample_data <- function(sample_data, original_headers = FALSE) {
  # make sure sample_data is the right type
  if ("qsip_data" %in% class(sample_data)) {
    sd <- sample_data@sample_data
  } else if ("qsip_sample_data" %in% class(sample_data)) {
    sd <- sample_data
  } else {
    stop(glue::glue("sample_data should be class <qsip_sample_data> or <qsip_data>, not {class(sample_data)[1]}"))
  }

  # if is not boolean
  if (!is.logical(original_headers)) {
    stop(glue::glue("original_headers should be TRUE/FALSE, not {class(original_headers)[1]}"))
  }

  if (isTRUE(original_headers)) {
    sd@data |>
      dplyr::rename(
        !!(sd@sample_id) := sample_id,
        !!(sd@source_mat_id) := source_mat_id,
        !!(sd@gradient_position) := gradient_position,
        !!(sd@gradient_pos_density) := gradient_pos_density,
        !!(sd@gradient_pos_amt) := gradient_pos_amt,
        !!(sd@gradient_pos_rel_amt) := gradient_pos_rel_amt
      )
  } else {
    sd@data
  }
}
