#'
#'
#' @export

plot_resampling_convergence = function(q) {

  message(glue::glue_col("{red This is an alpha function and is undergoing testing!}"))

  k <- purrr::map(
    c(5, 10, 25, 50, 100, 250, 500, 1000),
    \(i) run_resampling(q,
                        resamples = i,
                        with_seed = 17,
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
    dplyr:: mutate(L = (mean_resampled_EAF - lower) / mean_resampled_EAF,
                   U = (mean_resampled_EAF - upper) / mean_resampled_EAF) |>
    tidyr::pivot_longer(cols = c(L, U)) |>
    ggplot2:: ggplot(ggplot2::aes(x = n, y = value, color = name)) +
    ggplot2::geom_point(alpha = 0.3, pch = 21) +
    ggplot2::geom_smooth(formula = "y ~ x", method = "loess") +
    ggplot2::scale_fill_manual(values = c("L" = "red", "U" = "#037bcf")) +
    ggplot2::scale_color_manual(values = c("L" = "red", "U" = "#037bcf"))
}
