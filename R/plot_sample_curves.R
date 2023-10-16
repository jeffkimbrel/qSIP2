#' Plot qSIP sample data density curves
#'
#' @param sample_data (*qsip_sample_data or qsip_data*) Sample data that holds density and abundance values
#' @param source_data (*qsip_source_data*) Optional data required if `sample_data` is a `qsip_sample_data` object
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_sample_curves <- function(sample_data,
                               source_data = NULL,
                               colors = NULL) {

  if ("qsip_data" %in% class(sample_data)) {
    df = dplyr::left_join(sample_data@sample_data@data,
                   sample_data@source_data@data,
                   by = "source_mat_id")
  } else if ("qsip_sample_data" %in% class(sample_data)) {
    if (is.null(source_data)) {
      stop("If providing a qsip_sample_data object, you must also give a qsip_source_data object to the 'source_data' argument")
    } else{
      df = dplyr::left_join(sample_data@data, source_data@data, by = "source_mat_id")
    }
  } else {
    stop(glue::glue("ERROR: sample_data is an unexpected type ({class(sample_data)[1]})... it must be class qsip_sample_data or qsip_data"))
  }

  if (is.null(colors)) {
    colors = c("12C" = "cornflowerblue",
               "13C" = "firebrick",
               "14N" = "cornflowerblue",
               "15N" = "firebrick",
               "16O" = "cornflowerblue",
               "18O" = "firebrick")
  }

  WAD = df %>%
    dplyr::group_by(source_mat_id) %>%
    dplyr::summarise(WAD = weighted.mean(gradient_pos_density, gradient_pos_rel_amt))

  p = df |>
    dplyr::filter(!is.na(gradient_position)) |>
    dplyr::filter(gradient_pos_density > 1.5) |>
    dplyr::group_by(source_mat_id) |>
    ggplot2::ggplot(ggplot2::aes(x = gradient_pos_density,
                                 y = gradient_pos_rel_amt,
                                 color = isotope)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::facet_wrap(~source_mat_id) +
    ggplot2::geom_vline(data = WAD, linetype = 3, ggplot2::aes(xintercept = WAD))

  p
}




