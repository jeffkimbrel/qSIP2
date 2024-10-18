#' Get dataframe of resampled growth data
#'
#' This function will return a dataframe of the resampled growth data embedded in a `qsip_data`
#' object.
#'
#' @param qsip_data_object A `qsip_data` object that has been run through `run_growth_calculations()`
#'
#' @export
#'
#' @returns A dataframe of the resampled data

get_growth_data <- function(qsip_data_object) {
  is_qsip_growth(qsip_data_object, error = TRUE)

  resample <- NULL

  # bind variables
  observed <- NULL

  qsip_data_object@growth$rates |>
    dplyr::select(-observed) |>
    dplyr::filter(!is.na(resample))

}
