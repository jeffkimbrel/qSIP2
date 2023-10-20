#' Plot the results of filtering features
#'
#' After running the `run_feature_filter()` code, this function will produce two
#' plot detailing the consequences of the filtering. Plot A shows the retained and
#' removed features by their total tube relative abundance contribution, and plot B
#' shows the retained and removed features by the total count of each category. The
#' "zero fractions" shown in plot B are those entirely missing from the given source_mat_id
#' and are thus not going to be retained no matter what the `run_filter_feature()`
#' parameters are.
#'
#' @param qsip_data_object (*qsip_data*)
#' @param return_type (*string, default: combined*) Changes the return type from a combined plot (*combined*), list of individual plots (*individual*) or list of dataframes (*dataframe*)
#' @param colors (*strings*) An optional color palette
#'
#' @export
#'
#' @family "visualizations"
#'
#' @return Combined or individual plots, or the raw dataframes


plot_filter_gradient_position <- function(qsip_data_object,
                                          return_type = "combined",
                                          colors = NULL) {
  if (!return_type %in% c("combined", "individual", "dataframe")) {
    stop(glue::glue("ERROR: return_type is an unknown type ({return_type})"))
  }

  if (length(qsip_data_object@filter_results) == 0) {
    stop(glue::glue("ERROR: please run run_filter_feature() on qsip_data_object before plotting results"))
  }

  if (is.null(colors)) {
    colors <- c(
      "Fraction Passed" = "#2B92BE",
      "Fraction Filtered" = "#BE572B",
      "Zero Fractions" = "gray30"
    )
  }


  by_abundance_df <- qsip_data_object@filter_results$fraction_filtered |>
    dplyr::group_by(source_mat_id, fraction_call) |>
    dplyr::summarize(
      tube_rel_abundance = sum(tube_rel_abundance),
      .groups = "drop"
    ) |>
    dplyr::filter(fraction_call != "Zero Fractions")

  by_abundance <- by_abundance_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = source_mat_id,
      y = tube_rel_abundance,
      fill = fraction_call
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(tube_rel_abundance, accuracy = 0.1)),
      position = ggplot2::position_stack(vjust = .5),
      angle = 90
    ) +
    ggplot2::labs(
      x = "source_mat_id",
      y = "Tube Relative Abundance",
      fill = "Filtering Results"
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      breaks = c(0, .25, .50, .75, 1.00)
    ) +
    ggplot2::theme(axis.text.x = element_text(
      angle = 90, hjust = 1,
      vjust = 0.5
    ))


  by_count_df <- qsip_data_object@filter_results$fraction_filtered |>
    dplyr::group_by(source_mat_id, fraction_call) |>
    dplyr::tally() |>
    dplyr::arrange(fraction_call)

  by_count <- by_count_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = source_mat_id,
      y = n,
      fill = fraction_call
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = n),
      position = ggplot2::position_stack(vjust = .5),
      angle = 90
    ) +
    ggplot2::labs(
      x = "source_mat_id",
      y = "Feature Count",
      fill = "Filtering Results"
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme(axis.text.x = element_text(
      angle = 90, hjust = 1,
      vjust = 0.5
    ))

  if (return_type == "combined") {
    patchwork::wrap_plots(
      A = (by_abundance +
        ggplot2::theme(legend.position = "none")), # remove legend from A only in patchwork
      B = by_count
    ) +
      patchwork::plot_layout(guides = "collect") +
      patchwork::plot_annotation(
        tag_levels = "A",
        title = "Filtered features",
        subtitle = "A) By tube abundance, B) By feature count"
      )
  } else if (return_type == "individual") {
    list(
      "A" = by_abundance,
      "B" = by_count
    )
  } else if (return_type == "dataframe") {
    list(
      "A" = by_abundance_df,
      "B" = by_count_df
    )
  }
}
