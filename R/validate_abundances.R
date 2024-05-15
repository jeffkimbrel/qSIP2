#' Check the validity of a feature abundance table (internal)
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
  stopifnot("feature data type should be 'counts', 'coverage', 'normalized' or 'relative'" =  type %in% c("counts", "coverage", "normalized", "relative"))

  if (type == "relative") {
    totals <- data |>
      tidyr::pivot_longer(cols = dplyr::where(is.numeric)) |>
      dplyr::group_by(name) |>
      dplyr::summarise(S = sum(value)) |>
      dplyr::filter(S > 1.001) |>
      dplyr::pull(name) |>
      length()

    if (totals > 0) {
      stop("Some columns have a total relative abundance sum greater than 1", call. = FALSE)
    }
  }

  values <- data |>
    dplyr::select(-all_of(feature_id))

  if (any(values < 0)) {
    stop("Some numbers are negative", call. = FALSE)

  } else if (!all(values - floor(values) == 0)) {
    if (type == "counts") {
      stop("Some data are not integers", call. = FALSE)
    }

  # this was in here as a check, but it isn't clear when some data might pass
    # the integer check above, but then still not be numeric.
    # So, commenting out for now.
  # } else if (length(values) - length(dplyr::select_if(values, is.numeric)) > 0) {
  #   stop("Some data is not numeric", call. = FALSE)

  } else {
    return(NULL)
  }
}
