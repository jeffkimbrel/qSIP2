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
#'
add_taxonomy <- function(feature_object, taxa, feature_id) {

  # make sure feature_object is the right type
  if (!"qsip_feature_data" %in% class(feature_object)) {
    stop(glue::glue("feature_object should be class <qsip_feature_data>, not {class(feature_object)[1]})"), call. = FALSE)
  }


  # check that feature_id column exists
  if (feature_id %in% colnames(taxa)) {
    taxa <- taxa |>
      dplyr::rename("feature_id" := feature_id)
  } else {
    stop(glue::glue("{feature_id} column not found in taxonomy dataframe"), call. = FALSE)
  }

  feature_object_ids <- feature_object@data["feature_id"]
  taxa_ids <- taxa["feature_id"]

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
