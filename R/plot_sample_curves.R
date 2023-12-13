#' Plot qSIP sample data density curves
#'
#' @param sample_data (*qsip_data*) qSIP object
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_sample_curves <- function(qsip_data,
                               colors = NULL) {

  if ("qsip_data" %in% class(qsip_data)) {
    df <- qsip_data@tube_rel_abundance |>
      dplyr::left_join(qsip_data@sample_data@data |>
                         dplyr::select(sample_id, gradient_position), by = "sample_id") |>
      dplyr::left_join(qsip_data@source_data@data |>
                         dplyr::select(source_mat_id, isotope))
  } else {
    stop(glue::glue("sample_data should be class <qsip_data>, not {class(qsip_data)[1]}"))
  }

  if (is.null(colors)) {
    colors <- c(
      "12C" = "cornflowerblue",
      "13C" = "firebrick",
      "14N" = "cornflowerblue",
      "15N" = "firebrick",
      "16O" = "cornflowerblue",
      "18O" = "firebrick"
    )
  }

  WAD <- df %>%
    dplyr::group_by(source_mat_id) %>%
    dplyr::summarise(WAD = weighted.mean(gradient_pos_density, gradient_pos_rel_amt))

  p <- df |>
    dplyr::filter(!is.na(gradient_position)) |>
    dplyr::filter(gradient_pos_density > 1.5) |>
    #dplyr::group_by(source_mat_id) |>
    ggplot2::ggplot(ggplot2::aes(
      x = gradient_pos_density,
      y = gradient_pos_rel_amt,
      color = isotope
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::facet_wrap(~source_mat_id) +
    ggplot2::geom_vline(data = WAD, linetype = 3, ggplot2::aes(xintercept = WAD))

  p
}
