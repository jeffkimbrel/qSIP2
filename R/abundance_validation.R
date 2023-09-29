#' Check the validity of an abundance table
#'
#' This validation function will check the validity of an abundance table. It
#' can run standalone, but was written to be run when creating a qSIP2 abundance
#' table object. Therefore, a valid run of this function will return NULL rather
#' than a value.
#'
#' Rows should contain the unique taxa ids with a column designated with the id
#' argument. Each other column name should be a unique sample name.
#'
#' Validity checking includes making sure all data is numeric (except for the
#' taxa IDs), all numbers are integers, and no numbers are negative.
#'
#' @param data ASV/OTU table (dataframe)
#' @param id Column name with taxa IDs (string)
#'
#' @return Returns `NULL` if the values are valid, or a printed error
#'
#' @export


abundance_validation = function(data, id) {
  data = data |>
    dplyr::select(-id)

  if (length(data) - length(dplyr::select_if(data, is.numeric)) > 0) {
    stop("some data is not numeric")
  } else if (!all(data-floor(data)==0)) {
    stop("some data are not integers")
  } else if (any(data < 0)) {
    stop("some numbers are negative")
  } else {
    return(NULL)
  }

}
