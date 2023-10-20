#' Check the validity of a feature abundance table
#'
#' This validation function is an internal function will check the validity of a
#' feature abundance table.
#'
#' Rows should contain the unique taxa ids with a column designated with the id
#' argument. Each other column name should be a unique sample name.
#'
#' Validity checking includes making sure all data is numeric (except for the
#' feature IDs), all numbers are integers, and no numbers are negative.
#'
#' @param data (*dataframe*) ASV/OTU table
#' @param feature_id (*string*) Column name with feature IDs
#'
#' @returns Returns `NULL` if the values are valid, or a printed error
#'
#' @export


validate_abundances <- function(data, feature_id) {
  data <- data |>
    dplyr::select(-feature_id)

  if (length(data) - length(dplyr::select_if(data, is.numeric)) > 0) {
    stop("ERROR: Some data is not numeric")
  } else if (!all(data - floor(data) == 0)) {
    stop("ERROR: Some data are not integers")
  } else if (any(data < 0)) {
    stop("ERROR: Some numbers are negative")
  } else {
    return(NULL)
  }
}
