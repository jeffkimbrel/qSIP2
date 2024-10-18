#' Generate a source data frame from a sample data frame
#'
#' There may be situations where you have a verbose sample data frame that has all
#' of the data for both the samples and the source materials. This function attempts
#' to infer a source data frame from the sample data frame. It does this by grouping
#' by the source matrix ID and then checking to see if all of the values in each
#' column are the same.If they are, then that column is kept in the source data frame.
#' If not, then that column is dropped.
#'
#' @param sample_data (*dataframe*) A data frame with combined sample and source data
#' @param source_mat_id (*string*) The column with source_mat_id
#'
#' @returns A data frame with the inferred source data
#'
#' @export

infer_source_data = function(sample_data, source_mat_id) {
  cols_to_keep = sample_data |>
    dplyr::group_by(!!as.name(source_mat_id)) |>
    dplyr::summarise_all(dplyr::n_distinct) |>
    dplyr::select_if(~all(. == 1)) |>
    colnames()

  sample_data |>
    dplyr::select(source_mat_id, dplyr::all_of(cols_to_keep)) |>
    unique()
}
