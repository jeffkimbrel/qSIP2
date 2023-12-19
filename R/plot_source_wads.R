#' Plot the source WADs by isotope
#'
#' @param qsip_data (*qsip_data*) qSIP object
#' @param group (*string*) An optional grouping parameter to facet the y or x,y axes
#' @param color (*string*) An optional override to the default color palette
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_source_wads <- function(qsip_data, group = NULL, colors = NULL) {
  if ("qsip_data" %in% class(qsip_data)) {
    # if given a qsip_data object then get both the sample_data and source_data out
    source_data <- qsip_data@source_data
    sample_data <- qsip_data@sample_data
  } else {
    stop(glue::glue("qsip_data should be class<qsip_data>, not {class(qsip_data)[1]}"))
  }

  if (is.null(colors)) {
    colors <- c(
      "12C" = "cornflowerblue", "13C" = "firebrick",
      "14N" = "cornflowerblue", "15N" = "firebrick",
      "16O" = "cornflowerblue", "18O" = "firebrick"
    )
  }

  qsip_data@source_wads |>
    dplyr::filter(!is.na(WAD)) |> # filter unfractionated
    dplyr::left_join(source_data@data, by = "source_mat_id") |>
    ggplot2::ggplot(ggplot2::aes(color = isotope)) +
    ggplot2::geom_segment(y = 0, yend = 1, ggplot2::aes(x = WAD, xend = WAD), linewidth = 1) +
    ggplot2::facet_grid(paste(group, " ~ .", sep = "")) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(x = "Weighted Average Density")
}
