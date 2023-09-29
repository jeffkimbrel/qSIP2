#' qSIP feature table class
#'
#' A class to hold and validate a feature abundance table
#'
#' @slot data A dataframe or tibble
#' @slot feature_id Column name with unique taxa IDs
#'
#' @export
#' @family "qSIP classes"
#'
#' @keywords object
#' @returns A `qsip_feature_data` that holds a validated feature abundance table.

qsip_feature_data <- S7::new_class(
  "qsip_feature_data",
  properties = list(
    data = S7::class_data.frame,
    feature_id = S7::class_character,
    taxonomy = S7::class_data.frame
    ),
  constructor = function(data,
                         feature_id) {

    # rename columns to standardized names
    data = data |>
      dplyr::select(feature_id = feature_id,
                    dplyr::everything()) |>
      dplyr::ungroup()

    S7::new_object(S7::S7_object(),
                   data = data,
                   feature_id = feature_id,
                   taxonomy = data.frame())
  },
  validator = function(self) {
      if (any(duplicated(self@data['feature_id']))) {
        message(glue::glue("There appear to be duplicate ids in the {self@id} column"))
      }

      qSIP2::abundance_validation(self@data, 'feature_id')

    }
)




