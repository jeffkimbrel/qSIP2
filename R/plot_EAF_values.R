#' Plot EAF and confidence intervals
#'
#' This function plots the observed EAF values for each feature in the dataset.
#' The features are ordered by their observed EAF values. The confidence intervals
#' are plotted as error bars or ribbons. The points are colored based on the
#' success ratio of the resamples.
#'
#' @param qsip_data_object (*qsip_data*) A qsip_data object or list of qsip_data objects
#' @param confidence (*numeric*) The confidence level for the confidence interval
#' @param success_ratio (*numeric*) The ratio of successful resamples to total resamples
#' @param top (*numeric*) The number of top features to plot. Use `Inf` for all
#' @param error (*character*) The type of error bars to plot. Options are 'none', 'bar', 'ribbon'
#' @param alpha (*numeric*) The transparency of the error bar/ribbon
#' @param zero_line (*logical*) Add a line at EAF = 0
#'
#' @export

plot_EAF_values <- function(qsip_data_object,
                            confidence = 0.9,
                            success_ratio = 0.9,
                            top = Inf,
                            error = "none",
                            alpha = 0.3,
                            zero_line = TRUE) {

  # confirm qsip_data_object class is either qsip_data or list

  if (is_multi_qsip_data(qsip_data_object, error = FALSE)) {
    object_type = "multiple"
  } else if (is_qsip_data(qsip_data_object, error = FALSE)) {
    object_type = "single"
  } else {
    stop("ERROR: qsip_data_object must be of class <qsip_data> or <list> of qsip_data objects")
  }

  # confirm the confidence value is numeric and between 0-1
  stopifnot("ERROR: confidence should be numeric" = is.numeric(confidence))
  if (confidence >= 1 | confidence <= 0) {
    stop("ERROR: confidence level should be between 0 and 1")
  }

  # confirm the success_ratio value is numeric and between 0-1
  stopifnot("ERROR: success_ratio should be numeric" = is.numeric(success_ratio))
  if (success_ratio > 1 | success_ratio <= 0) {
    stop("ERROR: success_ratio level should be between 0 and 1")
  }

  # confirm the alpha value is numeric and between 0-1
  stopifnot("ERROR: alpha should be numeric" = is.numeric(alpha))
  if (alpha > 1 | alpha <= 0) {
    stop("ERROR: alpha level should be between 0 and 1")
  }

  if (!error %in% c('none', 'bar', 'ribbon')) {
    stop(glue::glue("<error> should be 'none', 'bar' or 'ribbon', not {error}"), call. = FALSE)
  }

  EAF <- summarize_EAF_values(qsip_data_object,
    confidence = confidence)

  # add number of attempted resamples
  if (object_type == "multiple") {

    EAF <- EAF |>
      dplyr::group_by(group) |>
      dplyr::slice_max(observed_EAF, n = top) |>
      dplyr::ungroup() |>
      dplyr::mutate(feature_id = tidytext::reorder_within(feature_id, observed_EAF, within = group)) %>%
      dplyr::left_join(sapply(qsip_data_object, n_resamples) |>
                         tibble::enframe(name = "group", value = "resamples"),
                       by = "group")

  } else {
    EAF = EAF |>
      dplyr::slice_max(observed_EAF, n = top) |>
      dplyr::mutate(resamples = qsip_data_object@resamples$n)

  }

  p = EAF |>
    dplyr::mutate(feature_id = forcats::fct_reorder(feature_id, observed_EAF)) |>
    ggplot2::ggplot(ggplot2::aes(y = feature_id, x = observed_EAF)) +
    ggplot2::geom_point(
      pch = 21,
      size = 2,
      ggplot2::aes(fill = ifelse((labeled_resamples + unlabeled_resamples) > resamples * 2 * success_ratio,
                                 "Passed",
                                 "Failed"
      ))
    ) +
    ggplot2::scale_fill_manual(values = c("Passed" = "#00ff0066", "Failed" = "red")) +
    ggplot2::labs(
      fill = glue::glue(">{success_ratio * 100}% successes"),
      y = "feature_ids reordered by observed_EAF"
    )

  if (error == "bar") {
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(xmin = lower, xmax = upper), alpha = alpha)
  } else if (error == "ribbon") {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(xmin = lower, xmax = upper, group = 1), alpha = alpha)
  }

  if (isTRUE(zero_line)) {
    p <- p +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  }

  if (object_type == "multiple") {
    p <- p +
      tidytext::scale_y_reordered() +
      ggplot2::facet_wrap(~group, scales = "free_y")
  }

  return(p)
}
