#' Plot occurrence of features in samples
#'
#' This is a plotting function to visualize the occurrence of features in samples.
#' The function takes a qsip_data object and a vector of feature_ids, and can scale
#' the results by total abundance or source abundance, and the WAD value can also be shown.
#'
#' @param qsip_data_object (*qsip_data*) A qsip_data object
#' @param feature_ids (*character*) A vector of feature_ids
#' @param scale (*character*) A character string
#' @param show_wad (*logical*) A logical value
#' @param title (*character*) A character string
#' @param legend.position (*character* or *numeric vector*) Values passed to ggplot2::theme(legend.position = ...)
#'
#' @export
#'
#' @returns Returns a ggplot object

plot_feature_occurrence = function(qsip_data_object,
                                   feature_ids,
                                   scale = "none",
                                   show_wad = FALSE,
                                   title = NULL,
                                   legend.position = "right") {

  # stop if qsip_data_object !is_qsip()
  if (!is_qsip_data(qsip_data_object)) {
    stop("Input must be a <qsip_data> object", call. = F)
  }

  # features must be a vector, and not too big
  if (!is.vector(feature_ids)) {
    stop("feature_ids must be a vector", call. = F)
  } else if (length(feature_ids) > 64) {
    stop("The length of feature_ids is capped at 64", call. = F)
  }

  # scale must be "none", "total", or "source"
  if (!scale %in% c("none", "total", "source")) {
    stop("scale must be 'none', 'total', or 'source'", call. = F)
  }

  # show_wad must be boolean
  if (!is.logical(show_wad)) {
    stop("show_wad must be a boolean", call. = F)
  }

  # title must be a character or NULL
  if (!is.character(title) & !is.null(title)) {
    stop("title must be a character or NULL", call. = F)
  }



  # make dataframe with joined metadata
  df = qsip_data_object@tube_rel_abundance |>
    dplyr::filter(feature_id %in% feature_ids) |>
    dplyr::left_join(qsip_data_object@sample_data@data,
              by = join_by(sample_id, source_mat_id, gradient_pos_density, gradient_pos_rel_amt)) |>
    dplyr::left_join(qsip_data_object@source_data@data,
              by = join_by(source_mat_id)) |>
    dplyr::left_join(qsip_data_object@wads,
              by = join_by(feature_id, source_mat_id))

  # if scale = "source"
  if (scale == "source") {
    df = df |>
      dplyr::mutate(tube_rel_abundance_source = tube_rel_abundance / sum(tube_rel_abundance),
                    .by = c("feature_id", "source_mat_id"))
  }

  # base plot
  p = df |>
    ggplot2::ggplot(ggplot2::aes(y = source_mat_id))

  # if show_wad is true
  if (isTRUE(show_wad)) {
    p = p +
      ggplot2::geom_point(pch = "|", size = 6,
                          ggplot2::aes(x = WAD, color = isotope)) +
      ggplot2::scale_color_manual(values = isotope_palette)
  }

  # choose which size to plot based on scale argument
  if (scale == "total") {
    p = p +
      ggplot2::geom_point(pch = 21, alpha = 0.9,
                          ggplot2::aes(x = gradient_pos_density, size = tube_rel_abundance, fill = isotope))
  } else if (scale == "source") {
    p = p +
      ggplot2::geom_point(pch = 21, alpha = 0.9,
                          ggplot2::aes(x = gradient_pos_density, size = tube_rel_abundance_source, fill = isotope))
  } else {
    p = p +
      ggplot2::geom_point(pch = 21, alpha = 0.9, size = 2,
                          ggplot2::aes(x = gradient_pos_density, fill = isotope))
  }


  # finish and return plot
  p +
    ggplot2::facet_wrap(~feature_id) +
    ggplot2::scale_fill_manual(values = isotope_palette) +
    ggplot2::labs(title = title) +
    ggplot2::theme(legend.position = legend.position)
}
