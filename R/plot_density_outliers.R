#' Cook's outlier detection on gradient positions vs densities
#'
#' Assuming a linear relationship between the gradient_position and gradient_pos_density,
#' this function will plot any potential outliers using Cook's distance and a defined
#' sensitivity. The lower the sensitivity, the more likely outliers will be flagged.
#'
#' @param sample_data (*qsip_sample_data or qsip_data*) A qsip object with sample data
#' @param sensitivity (*numeric, default: 4*) A sensitivity value, with lower values being more sensitive to outlier detection
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"


plot_density_outliers <- function(sample_data,
                                  sensitivity = 4) {

  if (inherits(sample_data, c("qsip_data", "qSIP2::qsip_data"))) {
    data <- sample_data@sample_data@data
  } else if (inherits(sample_data, c("qsip_sample_data", "qSIP2::qsip_sample_data"))) {
    data <- sample_data@data
  } else {
    stop(glue::glue("sample_data should be class <qsip_sample_data> or <qsip_data>, not {class(sample_data)[1]}"), call. = FALSE)
  }

  stopifnot("sensitivity should be a <numeric>" = is.numeric(sensitivity))

  if (any(data$gradient_position == -1)) {
    message("some unfractionated samples have been filtered from this plot")
  }

  # bind variables
  gradient_position <- source_mat_id <- . <- .cooksd <- COOKS_CUTOFF <- gradient_pos_density <- sample_id <- NULL


  data = data |>
    dplyr::filter(gradient_position > 0)

  S <- data |>
    dplyr::group_by(source_mat_id) |>
    dplyr::do(broom::augment(stats::lm(gradient_pos_density ~ gradient_position, data = .))) |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_"), COOKS_CUTOFF = sensitivity / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(S, .cooksd, COOKS_CUTOFF)

  potential_outliers <- S |>
    dplyr::filter(.cooksd >= COOKS_CUTOFF) |>
    dplyr::pull(S)

  data |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_")) |>
    dplyr::left_join(S, by = "S") |>
    ggplot2::ggplot(ggplot2::aes(x = gradient_position, y = gradient_pos_density)) +
    ggplot2::geom_smooth(method = "lm", formula = "y ~ x") +
    ggplot2::geom_point(ggplot2::aes(color = ifelse(.cooksd > COOKS_CUTOFF, "red", "gray30"))) +
    ggrepel::geom_text_repel(
      verbose = FALSE,
      na.rm = TRUE,
      size = 3,
      color = "gray30",
      ggplot2::aes(label = ifelse(.cooksd > COOKS_CUTOFF, sample_id, NA))
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::facet_wrap(~source_mat_id) +
    ggplot2::labs(
      title = "Cook's Distance Outlier Detection",
      subtitle = glue::glue("sensitivity = {sensitivity}")
    )
}
