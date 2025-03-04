#' Add gradient_pos_rel_amt to data
#'
#' This function will calculate the relative amount of a fraction compared to
#' the whole source using either qPCR copies or DNA concentrations. This works
#' by summing all of the <amt> within a source, and then dividing each fraction
#' by the total. The total percent will always equal 1 (or 100%) when running
#' this function.
#'
#' There may be times that you would want to provide values that do not sum to
#' 100%. For example, if you throw out a certain fraction because it didn't
#' sequence well, then your total could be less than 100%.
#'
#' @param data (*dataframe*) Sample metadata
#' @param amt (*string*) Column name that has the qPCR or DNA amounts per fraction
#' @param source_mat_id (*string, default: "source_mat_id"*) Column name with the source_mat_id
#' @param overwrite (*bool, default: FALSE*) Determines whether or not to overwrite an existing gradient_pos_rel_amt column
#'
#' @export
#'
#' @family "Sample Data"
#'
#' @returns A dataframe with a `gradient_pos_rel_amt` column

add_gradient_pos_rel_amt <- function(data,
                                     amt,
                                     source_mat_id = "source_mat_id",
                                     overwrite = FALSE) {

  # make sure column names exist
  if (!amt %in% colnames(data)) {
    stop(glue::glue("{amt} not found in dataframe"), call. = FALSE)
  }
  if (!source_mat_id %in% colnames(data)) {
    stop(glue::glue("{source_mat_id} not found in dataframe"), call. = FALSE)
  }


  if ("gradient_pos_rel_amt" %in% colnames(data)) {
    if (overwrite == FALSE) {
      stop("gradient_pos_rel_amt already exists! Set overwrite = TRUE if you want to overwrite", call. = FALSE)
    } else if (overwrite == TRUE) {
      message("WARNING: gradient_pos_rel_amt already exists and will be overwritten")
    }
  }

  data |>
    dplyr::mutate(
      gradient_pos_rel_amt = !!as.name(amt) / sum(!!as.name(amt)),
      .by = !!as.name(source_mat_id)
    )
}



#' Add a taxonomy table to qSIP abundance data
#'
#' @description
#'
#' This function is designed to add taxonomy data to a feature table, but in reality
#' it can hold any metadata that you want to associate with your features. The only
#' requirement is that the metadata table must have a feature id column with values
#' that match the feature ids in the `qsip_feature_data` object, and these ids must
#' not be duplicated.
#'
#' @param feature_object (*qsip_feature_data*) An object of `qsip_feature_data` class
#' @param taxa (*dataframe*) A taxa table
#' @param feature_id (*string*) The column name for the feature ids that match the ids in the
#' abundance table
#'
#' @export
#'
#' @family "Feature Data"
#'
#' @returns An updated `qsip_feature_data` with the taxonomy slot populated with a taxonomy dataframe.
#' @importFrom rlang :=

add_taxonomy <- function(feature_object, taxa, feature_id) {

  # make sure feature_object is the right type
  if (!inherits(feature_object, qsip_feature_data)) {
    stop(glue::glue("feature_object should be class <qsip_feature_data>, not {class(feature_object)[1]})"), call. = FALSE)
  }


  # check that feature_id column exists
  if (feature_id %in% colnames(taxa)) {
    taxa <- taxa |>
      dplyr::rename("feature_id" := feature_id)
  } else {
    stop(glue::glue("{feature_id} column not found in taxonomy dataframe"), call. = FALSE)
  }

  feature_object_ids <- feature_object@data["feature_id"] |> dplyr::pull(feature_id)
  taxa_ids <- taxa["feature_id"] |> dplyr::pull(feature_id)

  # check that feature ids are not duplicated
  if (any(duplicated(taxa_ids))) {
    stop("some feature_ids in the taxonomy dataframe are duplicated", call. = FALSE)
  }


  if (length(setdiff(feature_object_ids, taxa_ids)) > 0) {
    stop("Some ids found in the abundance object are not found in the taxa table", call. = FALSE)
  } else if (length(setdiff(taxa_ids, feature_object_ids)) > 0) {
    stop("Some ids found in the taxa table are not found in the abundance object", call. = FALSE)
  } else {
    feature_object@taxonomy <- taxa
    feature_object
  }
}




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
#'
#' @family "Sample Data"

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
