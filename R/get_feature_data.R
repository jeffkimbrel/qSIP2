#' Get feature dataframe from qSIP2 object
#'
#' This function will return the feature data stored in a qSIP2 object as a tibble.
#'
#' @param feature_data (*qsip_feature_data or qsip_data*) A qSIP2 object with feature data
#' @param original_headers (*boolean, default: FALSE*) If TRUE, the original column names will be used
#'
#' @export
#'
#' @returns A tibble with the feature data


get_feature_data <- function(feature_data, original_headers = FALSE) {
  # make sure feature_data is the right type
  if ("qsip_data" %in% class(feature_data)) {
    sd <- feature_data@feature_data
  } else if ("qsip_feature_data" %in% class(feature_data)) {
    sd <- feature_data
  } else {
    stop(glue::glue("feature_data should be class <qsip_feature_data> or <qsip_data>, not {class(feature_data)[1]}"))
  }

  # if is not boolean
  if (!is.logical(original_headers)) {
    stop(glue::glue("original_headers should be TRUE/FALSE, not {class(original_headers)[1]}"))
  }

  if (isTRUE(original_headers)) {
    sd@data |>
      dplyr::rename(!!(sd@feature_id) := feature_id)
  } else {
    sd@data
  }
}
