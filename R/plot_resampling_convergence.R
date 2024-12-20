#' Plot resampling convergence (under construction!)
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been resampled

plot_resampling_convergence = function(qsip_data_object) {

  message(glue::glue_col("{red This is an alpha function and is undergoing testing!}"))

  # bind variables
  mean_resampled_EAF <- lower <- upper <- L <- U <- n <- value <- name <- NULL

  k <- purrr::map(
    c(rep(1, 10), rep(2,10), rep(4,10),rep(8,10),rep(16,10), rep(32,10),rep(64,10),rep(128,10), rep(256,10), rep(512,10), rep(1024,10)),
    \(i) run_resampling(qsip_data_object,
                        resamples = i,
                        #with_seed = 17,
                        progress = FALSE,
                        allow_failures = TRUE,
                        quiet = T
    ) |>
      run_EAF_calculations() |>
      summarize_EAF_values(quiet = T) |>
      dplyr::mutate(n = i),
    .progress = TRUE
  )

  dplyr::bind_rows(k) |>
      ggplot2::ggplot(ggplot2::aes(x = unlabeled_resamples, y = mean_resampled_EAF)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~feature_id, scales = "free_y") +
      ggplot2::scale_x_log10() +
      ggplot2::geom_smooth(color = "#037bcf",
                  formula = 'y ~ x',
                  method = 'loess') +
      #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
      ggplot2::geom_hline(aes(yintercept = observed_EAF), color = "red") +
      ggplot2::geom_boxplot(ggplot2::aes(group = unlabeled_resamples))
}
