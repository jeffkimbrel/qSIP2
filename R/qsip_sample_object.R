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
#' @keywords sample_data



qsip_sample_object <- S7::new_class("qsip_sample_object",
                                properties = list(
                                  data = S7::class_data.frame,
                                  isotope = S7::new_property(S7::class_character, default = "isotope"),
                                  #isotopolog = S7::class_character,
                                  isotopolog_label = S7::class_character,
                                  #isotopolog_approach = S7::class_character,
                                  gradient_position = S7::new_property(S7::class_character, default = "gradient_position"),
                                  gradient_pos_density = S7::new_property(S7::class_character, default = "gradient_pos_density"),
                                  gradient_pos_amt = S7::new_property(S7::class_character, default = "gradient_pos_amt"),
                                  gradient_pos_rel_amt = S7::new_property(S7::class_character, default = "gradient_pos_rel_amt"),
                                  source_mat_id = S7::new_property(S7::class_character, default = "source_mat_id")
                                ),
                                validator = function(self) {
                                  qSIP2::isotope_validation(self@data |> dplyr::pull(self@isotope))
                                  qSIP2::gradient_pos_density_validation(self@data |> dplyr::pull(self@gradient_pos_density))
                                  qSIP2::gradient_position_validation(self@data |> dplyr::pull(self@gradient_position))
                                }
)






#' Plot qSIP sample data density curves
#'
#' @param x An object of `qsip_sample_object` class
#'
#' @export
#'
#' @keywords sample_data

plot_sample_curves <- S7::new_generic("plot_sample_curves", "x")

S7::method(plot_sample_curves, qsip_sample_object) <- function(x) {
  facet_formula = paste0("~", x@source_mat_id)

  x@data |>
    dplyr::filter(!is.na(!!as.name(x@gradient_position))) |>
    dplyr::filter(!!as.name(x@gradient_pos_density) > 1.5) |>
    dplyr::group_by(!!as.name(x@source_mat_id)) |>
    dplyr::mutate(j = !!as.name(x@gradient_pos_amt) / sum(!!as.name(x@gradient_pos_amt))) |>
    ggplot2::ggplot(aes(x = !!as.name(x@gradient_pos_density), y = j)) +
    ggplot2::geom_point(aes(color = !!as.name(x@isotope))) +
    ggplot2::geom_line(aes(color = !!as.name(x@isotope)), linewidth = 1) +
    ggplot2::scale_color_manual(values = c("16" = "lightblue", "18" = "blue", "16O" = "lightblue", "18O" = "blue", "15" = "yellow")) +
    ggplot2::facet_wrap(facet_formula, scales = "free")

}

#' Get sample counts from qSIP sample data
#'
#' @param x An object of `qsip_sample_object` class
#'
#' @export
#'
#' @keywords sample_data

get_sample_counts <- S7::new_generic("get_sample_counts", "x")

S7::method(get_sample_counts, qsip_sample_object) <- function(x) {
  x@data[x@source_mat_id] |>
    dplyr::rename(source_mat_id = x@source_mat_id) |>
    dplyr::count(source_mat_id)
}
