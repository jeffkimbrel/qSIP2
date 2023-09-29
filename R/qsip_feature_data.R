#' qSIP feature table class
#'
#' A class to hold and validate a feature abundance table
#'
#' @slot data A dataframe or tibble
#' @slot feature_id Column name with unique taxa IDs
#'
#' @export
#'
#' @keywords object

qsip_feature_data <- S7::new_class(
  "qsip_feature_data",
  properties = list(
    data = S7::class_data.frame,
    feature_id = S7::class_character,
    taxonomy = S7::class_data.frame
    ),
  constructor = function(data, feature_id, taxonomy = data.frame()) {

    # rename columns to standardized names
    data = data |>
      dplyr::select(feature_id = feature_id,
                    dplyr::everything()) |>
      dplyr::ungroup()

    S7::new_object(S7::S7_object(),
                   data = data,
                   feature_id = feature_id,
                   taxonomy = taxonomy)
  },
  validator = function(self) {
      if (any(duplicated(self@data['feature_id']))) {
        message(glue::glue("There appear to be duplicate ids in the {self@id} column"))
      }

      #qSIP2::abundance_validation(self@data, 'feature_id')

    }
)



#' Add a taxonomy table to qSIP abundance data
#'
#' @param x An object of `qsip_feature_data` class
#' @param taxa A taxa table
#' @param id The column name for the taxa ids that match the ids in the
#' abundance table
#'
#' @export
#'
#' @keywords abundance

add_taxonomy <- S7::new_generic("add_taxonomy", "x")

S7::method(add_taxonomy, qsip_feature_data) <- function(x, taxa, feature_id) {

  x_ids = x@data |> dplyr::pull(feature_id)
  taxa_ids = taxa |> dplyr::pull(feature_id)

  if (length(setdiff(x_ids, taxa_ids)) > 0) {
    stop("some ids found in the abundance object are not found in the taxa table")
  } else if (length(setdiff(taxa_ids, x_ids)) > 0) {
    setdiff(taxa_ids, x_ids)
    stop("some ids found in the taxa table are not found in the abundance object")
  } else {

    # rename taxa id column to the object level ID
    x@taxonomy = taxa |>
      dplyr::rename("feature_id" := feature_id)

    x
  }
}
