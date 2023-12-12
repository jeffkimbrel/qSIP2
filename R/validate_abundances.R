#' Check the validity of a feature abundance table
#'
#' This validation function is an internal function will check the validity of a
#' feature abundance table.
#'
#' Rows should contain the unique taxa ids with a column designated with the id
#' argument. Each other column name should be a unique sample name.
#'
#' Validity checking includes making sure all data is numeric (except for the
#' feature IDs), all numbers are integers (if `type = "counts"`), and no numbers are
#' negative.
#'
#' @param data (*dataframe*) ASV/OTU table
#' @param feature_id (*string*) Column name with feature IDs
#' @param type (*string*) *counts* requires integers, *coverage* and *relative* can take any positive numeric
#'
#' @returns Returns `NULL` if the values are valid, or a printed error
#'
#' @export


validate_abundances <- function(data,
                                feature_id,
                                type) {
  stopifnot("ERROR: feature data type should be 'counts', 'coverage', 'normalized' or 'relative'" =  type %in% c("counts", "coverage", "normalized", "relative"))

  if (type == "relative") {
    totals <- data |>
      tidyr::pivot_longer(cols = dplyr::where(is.numeric)) |>
      dplyr::group_by(name) |>
      dplyr::summarise(S = sum(value)) |>
      dplyr::filter(S > 1.001) |>
      dplyr::pull(name) |>
      length()

    if (totals > 0) {
      stop("ERROR: some columns have a total relative abundance sum greater than 1")
    }
  }

  values <- data |>
    dplyr::select(-all_of(feature_id))

  if (any(values < 0)) {
    stop("ERROR: Some numbers are negative")

  } else if (!all(values - floor(values) == 0)) {
    if (type == "counts") {
      stop("ERROR: Some data are not integers")
    }

  } else if (length(values) - length(dplyr::select_if(values, is.numeric)) > 0) {
    stop("ERROR: Some data is not numeric")

  } else {
    return(NULL)
  }
}
