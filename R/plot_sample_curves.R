#' Plot qSIP sample data density curves
#'
#' @param qsip_data_object (*qsip_data*) qSIP object
#' @param title (*character*) An optional title for the plot
#' @param facet_by (*character*) Facet the plots by "source" or by "isotope"
#' @param show_wad (*logical*) A logical value
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_sample_curves <- function(qsip_data_object,
                               title = NULL,
                               facet_by = "source",
                               show_wad = FALSE,
                               colors = lifecycle::deprecated()) {

  is_qsip_data(qsip_data_object, error = TRUE)

  # error if title is not a string
  if (!is.null(title) && !is.character(title)) {
    stop("title must be a character string", call. = FALSE)
  }

  # facet_by must be either "source" or "isotope"
  if (!facet_by %in% c("source", "isotope")) {
    stop("facet_by must be either 'source' or 'isotope'", call. = FALSE)
  }

  # show_wad must be boolean
  if (!is.logical(show_wad)) {
    stop("show_wad must be TRUE/FALSE", call. = F)
  }

  if (lifecycle::is_present(colors)) {
    lifecycle::deprecate_warn("0.18.3", "plot_sample_curves(colors)")
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
    dplyr::filter(gradient_position > 0) |>
    dplyr::summarize(tube_rel_abundance = sum(tube_rel_abundance), .by = c(sample_id, source_mat_id, gradient_position, gradient_pos_density, isotope))

  source_wads <- qsip_data_object@source_wads |>
    dplyr::filter(!is.na(WAD)) |>
    dplyr::left_join(qsip_data_object@source_data@data, by = "source_mat_id")

  p <- df |>
    dplyr::filter(!is.na(gradient_position)) |>
    #dplyr::filter(gradient_pos_density > 1.5) |>
    # dplyr::group_by(source_mat_id) |>
    ggplot2::ggplot(ggplot2::aes(
      x = gradient_pos_density,
      y = tube_rel_abundance
    ))

  if (facet_by == "source") {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = isotope)) +
      ggplot2::geom_line(linewidth = 1, ggplot2::aes(color = isotope)) +
      ggplot2::facet_wrap(~source_mat_id) +
      ggplot2::scale_color_manual(values = isotope_palette)


    if (isTRUE(show_wad)) {
      p = p +
        ggplot2::geom_vline(data = source_wads,
                            size = 1,
                            linetype = 2, ggplot2::aes(xintercept = WAD,
                                                       color = isotope))
    }




  } else if (facet_by == "isotope") {

    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = source_mat_id)) +
      ggplot2::geom_line(linewidth = 1, ggplot2::aes(group = source_mat_id, color = source_mat_id)) +
      ggplot2::facet_wrap(~isotope) +
      ggplot2::scale_color_manual(values = source_palette(nrow(source_wads)))

    if (isTRUE(show_wad)) {
      p = p +
        ggplot2::geom_vline(data = source_wads,
                        size = 1,
                        linetype = 2, ggplot2::aes(xintercept = WAD,
                                                   color = source_mat_id))
    }



  }

  if (!is.null(title)) {
    p = p + ggplot2::labs(title = title)
  }

  p
}
