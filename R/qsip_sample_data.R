#' qSIP sample data class
#'
#' A class to hold and validate sample data.
#'
#' @slot data (*dataframe*) Metadata for samples/fractions
#' @slot sample_id (*string*) The unique sample ID
#' @slot source_mat_id (*string*) The unique ID for the biological subject or replicate
#' @slot gradient_position (*string*) Column name with the fraction position
#' @slot gradient_pos_density (*string*) Column name with the gradient density
#' @slot gradient_pos_amt (*string*) Column name with a total amount per fraction, either
#' qPCR copies or DNA
#' @slot gradient_pos_rel_amt (*string*) Column name with the relative fraction abundance
#'  compared to the total
#' @slot fraction_volume (*string*) The volume loaded onto the column. Required if the `gradient_pos_amt` is reported as a concentration
#'
#' @export
#' @family "qSIP Objects"
#'
#' @returns A validated object of the `qsip_sample_data` type

qsip_sample_data <- S7::new_class(
  "qsip_sample_data",
  properties = list(
    data = S7::class_data.frame,
    sample_id = S7::class_character,
    source_mat_id = S7::class_character,
    gradient_position = S7::class_character,
    gradient_pos_density = S7::class_character,
    gradient_pos_amt = S7::class_character,
    gradient_pos_rel_amt = S7::class_character
  ),
  constructor = function(data,
                         sample_id,
                         source_mat_id,
                         gradient_position,
                         gradient_pos_density,
                         gradient_pos_amt,
                         gradient_pos_rel_amt) {


    # rename columns to standardized names
    data = data |>
      dplyr::select(sample_id = sample_id,
                    source_mat_id = source_mat_id,
                    gradient_position = gradient_position,
                    gradient_pos_density = gradient_pos_density,
                    gradient_pos_amt = gradient_pos_amt,
                    gradient_pos_rel_amt = gradient_pos_rel_amt,
                    dplyr::everything()) |>
      dplyr::ungroup()

    S7::new_object(S7::S7_object(),
                   data = data,
                   sample_id = sample_id,
                   source_mat_id = source_mat_id,
                   gradient_position = gradient_position,
                   gradient_pos_density = gradient_pos_density,
                   gradient_pos_amt = gradient_pos_amt,
                   gradient_pos_rel_amt = gradient_pos_rel_amt)
  },
  validator = function(self) {
    qSIP2::validate_gradient_pos_density(self@data |> dplyr::pull(gradient_pos_density))
    qSIP2::validate_gradient_position(self@data |> dplyr::pull(gradient_position))
  }
)



#' Get sample counts from qSIP sample data
#'
#' @param x An object of `qsip_sample_data` class
#'
#' @export
#'
#' @keywords sample_data

get_sample_counts <- S7::new_generic("get_sample_counts", "x")

S7::method(get_sample_counts, qsip_sample_data) <- function(x) {
  x@data[x@source_mat_id] |>
    dplyr::rename(source_mat_id = x@source_mat_id) |>
    dplyr::count(source_mat_id)
}
