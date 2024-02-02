#' Validate that a data.frame doesn't already contained standard column names (internal)
#'
#' Currently used when instantiating one of the three "primal" objects. It will give an
#' error if the user tries to pass a non-standard column name, but an existing column name already
#' uses the standard name.
#'
#' @param data A data.frame
#' @param name The column name selected by the user
#' @param type The type of data (source, sample, or feature)
#'
#' @export
#'

validate_standard_names = function(data, name, type) {

  if (type == "source") {
    standard = "source_mat_id"
  } else if (type == "sample") {
    standard = "sample_id"
  } else if (type == "feature") {
    standard = "feature_id"
  } else {
    stop(glue::glue("The 'type' argument must be one of 'source', 'sample', or 'feature'. You passed <{type}>."), call. = FALSE)
  }

  if (!name == standard & standard %in% colnames(data)) {
    stop(glue::glue("You are trying to pass the <{name}> column as the '{standard}',
                    but a '{standard}' column already exists in your {type} dataframe.

                    If you really want to use the <{name}> column, it is recommended to
                    rename the existing '{standard}' column, or completely remove it from
                    your dataframe first to avoid collision issues.

                    Sorry for the inconvenience."), call. = FALSE)
  }
}
