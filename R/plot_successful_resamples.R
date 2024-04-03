#' Plot the number of successful resamples for each feature_id
#'
#' This function will plot the number of successful resamples for each feature_id.
#' This value will typically be the number of resamples given to `run_resampling()`,
#' but if `run_resampling()` is called with `allow_failures = TRUE` then the number
#' of successful resamples might be less than the number of resamples given.
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been resampled
#' @param as_percentage (*logical*) If TRUE, the x axis will be the percentage of the total number of resamples
#' @param labels (*logical*) If TRUE, the counts will be labeled on the plot
#'
#' @export
#'
#' @returns A ggplot object

plot_successful_resamples <- function(qsip_data_object,
                                      labels = FALSE,
                                      as_percentage = FALSE) {

  p <- get_resample_counts(qsip_data_object, as_percentage = as_percentage) |>
    tidyr::pivot_longer(cols = c("unlabeled_resamples", "labeled_resamples"),
                        names_to = "type",
                        values_to = "n") |>
    #dplyr::select(feature_id, type, successes = n) |>
    ggplot2::ggplot(ggplot2::aes(x = n, fill = type)) +
    ggplot2::geom_histogram(bins = 20) +
    ggplot2::facet_wrap(~type, ncol = 1) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Successes", y = "feature_id Count") +
    ggplot2::scale_fill_manual(values = c("labeled_resamples" = "#ff0000", "unlabeled_resamples" = "#037bcf"))

  if (isTRUE(labels)) {
    p <- p +
      ggplot2::stat_bin(
        bins = 20, ggplot2::aes(
          y = ggplot2::after_stat(count),
          label = ifelse(ggplot2::after_stat(count) == 0, "", ggplot2::after_stat(count))
        ),
        geom = "text", vjust = -.5
      )
  }

  return(p)
}
