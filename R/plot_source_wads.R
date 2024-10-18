#' Plot the source WADs by isotope
#'
#' @param qsip_data (*qsip_data*) qSIP object
#' @param group (*character*) An optional grouping parameter to facet the y or x,y axes
#' @param title (*character*) An optional title for the plot
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_source_wads <- function(qsip_data,
                             group = NULL,
                             title = NULL) {

  is_qsip_data(qsip_data, error = TRUE)

  # bind variables
  WAD <- isotope <- NULL

  source_data <- qsip_data@source_data
  sample_data <- qsip_data@sample_data

  # error if group is not a column name in source_data
  if (!is.null(group) && !group %in% colnames(source_data@data)) {
    stop("group must be a column name in source_data", call. = FALSE)
  }

  p = qsip_data@source_wads |>
    dplyr::filter(!is.na(WAD)) |> # filter unfractionated
    dplyr::left_join(source_data@data, by = "source_mat_id") |>
    ggplot2::ggplot(ggplot2::aes(color = isotope)) +
    ggplot2::geom_segment(y = 0, yend = 1, ggplot2::aes(x = WAD, xend = WAD), linewidth = 1) +
    ggplot2::facet_grid(paste(group, " ~ .", sep = "")) +
    ggplot2::scale_color_manual(values = isotope_palette) +
    ggplot2::labs(x = "Weighted Average Density")

  if (!is.null(title)) {
    p = p + ggplot2::labs(title = title)
  }

  p
}
