#' qSIP source data class
#'
#' A class to hold and validate qSIP source data.
#'
#' This `qSIP2` object holds source data for calculating EAF values. If you want
#' to also calculate growth then you should use the `qsip_source_data_growth`
#' object.
#'
#' @slot data (*dataframe*) Source metadata
#' @slot isotope (*string*) Isotope name
#' @slot isotopolog (*string*)  Isotopolog data
#' @slot source_mat_id (*string*) The unique ID for the biological subject or source
#'
#' @export
#'
#' @family "qSIP Objects"
#'
#' @return A validated `qsip_source_data` object

qsip_source_data <- S7::new_class(
  "qsip_source_data",
  properties = list(
    data = S7::class_data.frame,
    isotope = S7::class_character,
    isotopolog = S7::class_character,
    source_mat_id = S7::class_character
  ),
  constructor = function(data,
                         isotope,
                         isotopolog,
                         source_mat_id) {


    # rename columns to standardized names
    data = data |>
      dplyr::select(isotope = isotope,
                  isotopolog = isotopolog,
                  source_mat_id = source_mat_id,
                  dplyr::everything()) |>
      dplyr::ungroup()

    S7::new_object(S7::S7_object(),
                   data = data,
                   isotope = isotope,
                   isotopolog = isotopolog,
                   source_mat_id = source_mat_id)
  },
  validator = function(self) {

    if (any(duplicated(self@data["source_mat_id"]))) {
      stop("ERROR: some source_mat_ids are duplicated")
    }

    qSIP2::isotope_validation(self@data |> dplyr::pull(isotope))
  }
)
