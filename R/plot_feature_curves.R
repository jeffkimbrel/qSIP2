#' Plot qSIP feature data density curves
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object with tube relative abundances
#' @param feature_ids (*string*) Feature ids to be plotted on their own facet
#' @param source_mat_ids (*string, defaults to all*) A list of source material ids
#' @param title (*string*) An optional title for the plot
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_feature_curves <- function(qsip_data_object,
                                feature_ids,
                                source_mat_ids = NULL,
                                title = NULL) {

  is_qsip_data(qsip_data_object, error = TRUE)

  # make sure all values in source_mat_ids are found in get_source_mat_ids(qsip_data_object)
  if (!is.null(source_mat_ids)) {
    if (length(setdiff(source_mat_ids, get_source_mat_ids(qsip_data_object))) > 0) {
      stop("some provided source_mat_ids are not found in the qsip_data object", call. = F)
    }
  }


  # get all source_mat_ids if NULL
  if (is.null(source_mat_ids)) {
    source_mat_ids <- qsip_data_object@source_data@data$source_mat_id
  }


  if (length(setdiff(feature_ids, qsip_data_object@feature_data@data$feature_id)) > 0) {
    stop("some provided feature_ids are not found in the qsip_data object", call. = F)
  }

  # bind variables
  source_mat_id <- isotope <- feature_id <- gradient_pos_density <- tube_rel_abundance <- NULL

  # get source data for isotope
  s_data <- qsip_data_object@source_data@data |>
    dplyr::select(source_mat_id, isotope)

  p <- qsip_data_object@tube_rel_abundance |>
    dplyr::left_join(s_data, by = "source_mat_id") |>
    dplyr::filter(source_mat_id %in% source_mat_ids) |>
    dplyr::filter(feature_id %in% feature_ids) |>
    ggplot2::ggplot(ggplot2::aes(
      x = gradient_pos_density,
      y = tube_rel_abundance,
      color = isotope
    )) +
    ggplot2::geom_line(ggplot2::aes(group = source_mat_id)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~feature_id, scales = "free_y") +
    ggplot2::scale_color_manual(values = c(
      "12C" = "#037bcf", "13C" = "#ff0000",
      "14C" = "#037bcf", "15N" = "#ff0000",
      "16O" = "#037bcf", "18O" = "#ff0000"
    ))

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }

  p
}
