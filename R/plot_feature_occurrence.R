#' Plot occurrence of features in samples
#'
#' This is a plotting function to visualize the occurrence of features in samples.
#' The function takes a qsip_data object and a vector of feature_ids, and can scale
#' the results by total abundance or source abundance, and the WAD value can also be shown.
#'
#' @param qsip_data_object (*qsip_data*) A qsip_data object
#' @param feature_ids (*character*) An optional vector of feature_ids
#' @param scale (*character*) A character string
#' @param show_wad (*logical*) A logical value
#' @param title (*character*) A character string
#' @param legend.position (*character* or *numeric vector*) Values passed to ggplot2::theme(legend.position = ...)
#'
#' @export
#'
#' @returns Returns a ggplot object


plot_feature_occurrence = function(qsip_data_object,
                                   feature_ids = NULL,
                                   scale = "none",
                                   show_wad = FALSE,
                                   title = NULL,
                                   legend.position = "right") {

  # stop if qsip_data_object !is_qsip()
  if (!is_qsip_data(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = F)
  }

  # feature_ids must be null or a vector of strings
  if (!is.null(feature_ids) & !is.character(feature_ids)) {
    stop("<feature_ids> argument must be NULL or a vector of strings", call. = FALSE)
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
  } else if (length(title) > 1) {
    stop("title should only have a length one 1", call. = F)
  }

  # if feature_ids is null, then get all
  if (is.null(feature_ids)) {
    feature_ids = get_feature_ids(qsip_data_object, filtered = T)
  }

  # if the sample_data contains an isotope column, remove it so there isn't a .x, .y
  sample_df = qsip_data_object@sample_data@data |> select(-dplyr::any_of("isotope"))

  # make dataframe with joined metadata
  df = qsip_data_object@tube_rel_abundance |>
    dplyr::filter(feature_id %in% feature_ids) |>
    dplyr::left_join(sample_df,
              by = dplyr::join_by(sample_id, source_mat_id, gradient_pos_density, gradient_pos_rel_amt)) |>
    dplyr::left_join(qsip_data_object@source_data@data,
              by = dplyr::join_by(source_mat_id)) |>
    dplyr::left_join(qsip_data_object@wads,
              by = dplyr::join_by(feature_id, source_mat_id))

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
