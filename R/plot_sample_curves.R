#' Plot qSIP sample data density curves
#'
#' @param qsip_data_object (*qsip_data*) qSIP object
#' @param colors (*character*) A named vector of colors for each isotope (optional)
#' @param title (*character*) An optional title for the plot
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_sample_curves <- function(qsip_data_object,
                               colors = NULL,
                               title = NULL) {

  is_qsip_data(qsip_data_object, error = TRUE)

  # error if title is not a string
  if (!is.null(title) && !is.character(title)) {
    stop("title must be a character string", call. = FALSE)
  }


  # bind variables
  sample_id <- gradient_position <- source_mat_id <- isotope <- WAD <- gradient_pos_density <- gradient_pos_rel_amt <- NULL

  df <- qsip_data_object@tube_rel_abundance |>
    dplyr::left_join(
      qsip_data_object@sample_data@data |>
        dplyr::select(sample_id, gradient_position),
      by = "sample_id"
    ) |>
    dplyr::left_join(
      qsip_data_object@source_data@data |>
        dplyr::select(source_mat_id, isotope),
      by = "source_mat_id"
    )

  if (any(df$gradient_position == -1)) {
    message("some unfractionated samples have been filtered from this plot")
  }

  df <- df |>
    dplyr::filter(gradient_position > 0)

  source_wads <- qsip_data_object@source_wads |>
    dplyr::filter(!is.na(WAD))

  if (is.null(colors)) {
    colors <- isotope_palette
  }

  p <- df |>
    dplyr::filter(!is.na(gradient_position)) |>
    dplyr::filter(gradient_pos_density > 1.5) |>
    # dplyr::group_by(source_mat_id) |>
    ggplot2::ggplot(ggplot2::aes(
      x = gradient_pos_density,
      y = gradient_pos_rel_amt,
      color = isotope
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::facet_wrap(~source_mat_id) +
    ggplot2::geom_vline(data = source_wads,
                        linetype = 3, ggplot2::aes(xintercept = WAD))

  if (!is.null(title)) {
    p = p + ggplot2::labs(title = title)
  }

  p
}
