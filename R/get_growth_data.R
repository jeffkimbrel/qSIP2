#' Get dataframe of resampled growth data
#'
#' This function will return a dataframe of the resampled growth data embedded in a `qsip_data`
#' object.
#'
#' @param qsip_data_object A `qsip_data` object that has been run through `run_growth_calculations()`
#' @param type (*string*) The type of data to return: "all", "unlabeled", or "labeled"
#'
#' @export
#'
#' @returns A dataframe of the resampled data

get_growth_data <- function(qsip_data_object,
                              type = "all") {
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  } else if (is.null(qsip_data_object@growth$rates)) {
    stop("this function requires a qsip object that has been run through run_growth_calculations()", call. = FALSE)
  }

  # bind variables
  observed <- NULL

  qsip_data_object@growth$rates |>
    dplyr::select(-observed)

}
