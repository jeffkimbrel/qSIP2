#' Plot the resampled EAFs for each feature
#'
#' This plot will show the results of the resampling procedure for each feature.
#' The resampling procedure is run using the run_resampling() function. The plot
#' will show the mean resampled EAF for each feature, with the confidence interval
#' (default 90%) shown as a bar or line (default no line). The area under the curve
#' can also be shown (default TRUE).
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been resampled
#' @param feature_ids (*character vector*) A vector of feature ids to filter on
#' @param area (*boolean*) Whether to plot the area under the curve or not (default: TRUE)
#' @param confidence (*numeric*) The confidence interval to plot
#' @param intervals (*character*) Whether to plot the confidence interval as a bar, line or not at all (default)
#'
#' @export
#'
#' @returns A ggplot object

plot_feature_resamplings <- function(qsip_data_object,
                                     feature_ids = NULL,
                                     area = TRUE,
                                     confidence = 0.9,
                                     intervals = "") {
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  } else if (length(qsip_data_object@resamples) == 0) {
    stop("this function requires a qsip object that has been run through run_resampling()", call. = FALSE)
  }

  # feature_ids must be null or a vector of strings
  if (!is.null(feature_ids) & !is.character(feature_ids)) {
    stop("<feature_ids> argument must be NULL or a vector of strings", call. = FALSE)
  }

  # area must be true or false
  if (!isTRUE(area) & !isFALSE(area)) {
    stop("<area> argument must be TRUE or FALSE", call. = FALSE)
  }

  # confidence must be between 0 and including 1
  if (confidence <= 0 | confidence > 1) {
    stop("<confidence> argument must be between 0 and 1", call. = FALSE)
  }

  unlabeled_data <- dplyr::bind_rows(qsip_data_object@resamples$u) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("unlabeled_")) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(feature_id, resample, type) |>
    dplyr::summarize(mean_resampled_WAD = mean(value))

  labeled_data <- dplyr::bind_rows(qsip_data_object@resamples$l) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("labeled_")) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(feature_id, resample, type) |>
    dplyr::summarize(mean_resampled_WAD = mean(value))

  combined_data <- rbind(unlabeled_data, labeled_data)

  # filter feature ids is given
  if (!is.null(feature_ids)) {
    combined_data <- combined_data |>
      dplyr::filter(feature_id %in% feature_ids)
  }

  p <- combined_data |>
    ggplot2::ggplot(ggplot2::aes(x = mean_resampled_WAD, y = type)) +
    ggplot2::facet_wrap(~feature_id, scales = "free_x") +
    ggplot2::scale_color_manual(values = c("labeled" = "#ff0000", "unlabeled" = "#037bcf")) +
    ggplot2::scale_fill_manual(values = c("labeled" = "#FF000055", "unlabeled" = "#037bcf55"))

  if (isTRUE(area)) {
    p <- p +
      ggridges::geom_density_ridges(ggplot2::aes(fill = type, height = ggplot2::after_stat(density)), stat = "density")
  }

  # options to add confidence interval data
  summary_statistics <- combined_data |>
    dplyr::group_by(feature_id, type) |>
    dplyr::summarize(
      mean_resampled_WAD2 = mean(mean_resampled_WAD),
      lower = quantile(mean_resampled_WAD, (1 - confidence) / 2, na.rm = T),
      upper = quantile(mean_resampled_WAD, 1 - (1 - confidence) / 2, na.rm = T),
      .groups = "drop"
    ) |>
    dplyr::rename(mean_resampled_WAD = mean_resampled_WAD2)

  if (intervals == "bar") {
    p <- p +
      ggplot2::geom_errorbarh(data = summary_statistics, ggplot2::aes(xmin = lower, xmax = upper, color = type), linewidth = 1, show.legend = F)
  } else if (intervals == "line") {
    p <- p +
      ggplot2::geom_vline(data = summary_statistics, ggplot2::aes(xintercept = mean_resampled_WAD, color = type), show.legend = F) +
      ggplot2::geom_vline(data = summary_statistics, ggplot2::aes(xintercept = lower, color = type), linetype = 2, show.legend = F) +
      ggplot2::geom_vline(data = summary_statistics, ggplot2::aes(xintercept = upper, color = type), linetype = 2, show.legend = F)
  }

  p
}
