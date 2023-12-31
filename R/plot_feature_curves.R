#' Plot qSIP feature data density curves
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object with tube relative abundances
#' @param feature_ids (*string*) Feature ids to be plotted on their own facet
#' @param source_mat_ids (*string, defaults to all*) A list of source material ids
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_feature_curves <- function(qsip_data_object,
                                feature_ids,
                                source_mat_ids = NULL) {
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop(glue::glue("qsip_data_object should be class <qsip_data>, not {class(qsip_data_object)[1]}"))
  }

  # get all source_mat_ids if NULL
  if (is.null(source_mat_ids)) {
    source_mat_ids <- qsip_data_object@source_data@data$source_mat_id
  }


  if (length(setdiff(feature_ids, qsip_data_object@feature_data@data$feature_id)) > 0) {
    message("WARNING: some feature_ids are not found")
  }



  # get source data for isotope
  s_data <- qsip_data_object@source_data@data |>
    dplyr::select(source_mat_id, isotope)

  qsip_data_object@tube_rel_abundance |>
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
    ggplot2::scale_color_manual(values = c("12C" = "cornflowerblue", "13C" = "firebrick",
                                           "14C" = "cornflowerblue", "15N" = "firebrick",
                                           "16O" = "cornflowerblue", "18O" = "firebrick"))
}
