#' qSIP sample data class
#'
#' A class to hold and validate sample data.
#'
#' @slot data A dataframe or tibble
#' @slot isotope Column name with the MISIP isotope data
#' @slot isotopolog Column name with the MISIP isotopolog data
#'
#' @export
#'

qsip_sample_object <- S7::new_class("qsip_sample_object",
                                properties = list(
                                  data = S7::class_data.frame,
                                  isotope = S7::class_character,
                                  isotopolog = S7::class_character,
                                  isotopolog_label = S7::class_character,
                                  isotopolog_approach = S7::class_character,
                                  gradient_position = S7::new_property(S7::class_character, default = "gradient_position"),
                                  gradient_pos_density = S7::new_property(S7::class_character, default = "gradient_pos_density"),
                                  gradient_pos_amt = S7::new_property(S7::class_character, default = "gradient_pos_amt"),
                                  gradient_pos_rel_amt = S7::new_property(S7::class_character, default = "gradient_pos_rel_amt"),
                                  source_mat_id = S7::new_property(S7::class_character, default = "source_mat_id")
                                ),
                                validator = function(self) {
                                  qSIP2::isotope_validation(self@data %>% pull(self@isotope))
                                  qSIP2::gradient_pos_density_validation(self@data %>% pull(self@gradient_pos_density))
                                  qSIP2::gradient_position_validation(self@data %>% pull(self@gradient_position))
                                }
)

#' Get sample counts from qSIP sample data
#'
#' @param x An object of `qsip_sample_object` class
#'
#' @export
#'

get_sample_counts <- S7::new_generic("get_sample_counts", "x")

S7::method(get_sample_counts, qsip_sample_object) <- function(x) {
  x@data[x@source_mat_id] |>
    dplyr::rename(source_mat_id = x@source_mat_id) |>
    dplyr::count(source_mat_id)
}
