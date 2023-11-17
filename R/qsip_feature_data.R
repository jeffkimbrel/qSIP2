#' qSIP feature table class
#'
#' A class to hold and validate a feature abundance table.
#'
#' This is a constructor function that makes the qSIP2 `qsip_feature_data` object.
#' It requires a dataframe/tibble (the `data` argument) that has the feature IDs as
#' a column designated with the `feature_id` argument. Each row corresponds to a unique
#' feature (amplicon, MAG, etc) and each subsequent row corresponds to a unique sample.
#'
#' There are several validation checks run on the data in the dataframe. The values must
#' be non-negative numerics. By default, the `type = abundance` argument strictly requires
#' the values be integers, but this requirement is relaxed if setting `type = coverage` or
#' `type = relative`.
#'
#' @slot data (*dataframe*) ASV/OTU table or equivalent
#' @slot feature_id (*string*) Column name with unique taxa IDs
#' @slot type (*string, default: counts*) The type of numerical data, either *counts*, *coverage* or *relative*
#'
#' @export
#' @family "qSIP Objects"
#'
#' @returns A validated object of the `qsip_feature_data` type

qsip_feature_data <- S7::new_class(
  "qsip_feature_data",
  properties = list(
    data = S7::class_data.frame,
    feature_id = S7::class_character,
    taxonomy = S7::class_data.frame,
    type = S7::class_character
  ),
  constructor = function(data,
                         feature_id = "feature_id",
                         type = "counts") {

    if (!"data.frame" %in% class(data)) {
      stop(glue::glue("data must be class <dataframe>, not {class(data)[1]}"), call. = FALSE)
    }

    if (!feature_id %in% colnames(data)) {
      stop(glue::glue("{feature_id} not found in dataframe"), call. = FALSE)
    }

    # rename columns to standardized names
    data <- data |>
      dplyr::select(
        feature_id = all_of(feature_id),
        dplyr::everything()
      ) |>
      dplyr::ungroup()

    S7::new_object(S7::S7_object(),
      data = data,
      feature_id = feature_id,
      taxonomy = data.frame(),
      type = type
    )
  },
  validator = function(self) {
    if (any(duplicated(self@data["feature_id"]))) {
      stop(glue::glue("There appear to be duplicate ids in the {self@feature_id} column"), call. = FALSE)
    }

    if (!self@type %in% c("counts", "coverage", "relative")) {
      stop(glue::glue("feature data type should be 'counts', 'coverage' or 'relative', not '{self@type}'"), call. = FALSE)
    }

    qSIP2::validate_abundances(self@data, "feature_id", type = self@type)
  }
)
