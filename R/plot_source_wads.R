#' Plot the source WADs by isotope
#'
#' @param sample_data (*qsip_data or qsip_sample_data*) A qsip object with sample data and optional source data
#' @param source_data (*qsip_source_data, optional*) An optional qsip_source_data object. Required if `sample_data` does not contain the source_data
#' @param group (*string*) An optional grouping parameter to facet the y or x,y axes
#' @param color (*string*) An optional override to the default color palette
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_source_wads = function(sample_data, source_data = NULL, group = NULL, colors = NULL) {

  if ("qsip_data" %in% class(sample_data)) {
    # if given a qsip_data object then get both the sample_data and source_data out
    source_data = sample_data@source_data
    sample_data = sample_data@sample_data

  } else if ("qsip_sample_data" %in% class(sample_data)) {
    # if given a sample_data object, then make sure the source_data is also given and is of the right class

    if (is.null(source_data)) {
      stop("ERROR: If providing a qsip_sample_data object, you must also give a qsip_source_data object to the 'source_data' argument")
    } else if (!"qsip_source_data" %in% class(source_data))  {
      stop(glue::glue("ERROR: source_data is an unexpected type ({class(source_data)[1]})... it must be class qsip_source_data"))
    }
  } else {
    stop(glue::glue("ERROR: sample_data is an unexpected type ({class(sample_data)[1]})... it must be class qsip_sample_data or qsip_data"))
  }


  if (is.null(colors)) {
    colors = c("12C" = "cornflowerblue", "13C" = "firebrick",
      "14N" = "cornflowerblue", "15N" = "firebrick",
      "16O" = "cornflowerblue", "18O" = "firebrick")
  }

  w = calculate_source_wads(sample_data)

  w |>
    dplyr::left_join(source_data@data, by = "source_mat_id") |>
    ggplot2::ggplot(ggplot2::aes(color = isotope)) +
    ggplot2::geom_segment(y = 0, yend = 1, ggplot2::aes(x = WAD, xend = WAD), linewidth = 1) +
    ggplot2::facet_grid(paste(group, " ~ .", sep = "")) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(x = "Weighted Average Density")


}






