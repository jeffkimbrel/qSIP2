#' Add a taxonomy table to qSIP abundance data
#'
#' @param feature_object (*qsip_feature_data*) An object of `qsip_feature_data` class
#' @param taxa (*dataframe*) A taxa table
#' @param id (*string*) The column name for the taxa ids that match the ids in the
#' abundance table
#'
#' @export
#'
#' @family "Feature Data"
#'
#' @returns An updated `qsip_feature_data` with the taxonomy slot populated with a taxonomy dataframe.

add_taxonomy <- function(feature_object, taxa, feature_id) {

  taxa = taxa |>
    dplyr::rename("feature_id" := feature_id)


  feature_object_ids = feature_object@data['feature_id']
  taxa_ids = taxa['feature_id']

  if (length(setdiff(feature_object_ids, taxa_ids)) > 0) {
    stop("Some ids found in the abundance object are not found in the taxa table")
  } else if (length(setdiff(taxa_ids, feature_object_ids)) > 0) {
    setdiff(taxa_ids, feature_object_ids)
    stop("Some ids found in the taxa table are not found in the abundance object")
  } else {

    feature_object@taxonomy = taxa
    feature_object
  }
}
