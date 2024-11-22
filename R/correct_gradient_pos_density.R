#' Correct sample density values using gradient position information
#'
#' Used in conjunction with `plot_density_outliers()`, this function will correct
#' the gradient_pos_density values for outliers based on the gradient_position
#' values. A linear model is built using the gradient_position and
#' gradient_pos_density, and if the Cook's outlier value is above the cutoff, then
#' that sample's gradient_pos_density value is replaced with the fitted value. If
#' it isn't above the cut-off, then no correction is made for that sample.
#'
#' @param sample_data (*qsip_sample_data*) A qsip object with sample data
#' @param sensitivity (*numeric, default: 4*) A sensitivity value, with lower values being more sensitive to outlier detection and correction
#'
#' @export
#'
#' @returns A qsip_sample_data object with corrected gradient_pos_density values

correct_gradient_pos_density <- function(sample_data,
                                         sensitivity = 4) {

  stopifnot("sample_data should be of class <qsip_sample_data>" = inherits(sample_data, qsip_sample_data))

  # sensitivity should a positive number
  stopifnot("sensitivity should be a positive number" = is.numeric(sensitivity) & sensitivity > 0)


  data = sample_data@data |>
    dplyr::filter(gradient_position > 0)

  S <- data |>
    dplyr::group_by(source_mat_id) |>
    dplyr::do(broom::augment(stats::lm(gradient_pos_density ~ gradient_position, data = .))) |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_"), COOKS_CUTOFF = sensitivity / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(S, .cooksd, COOKS_CUTOFF, .fitted)

  data |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_")) |>
    dplyr::left_join(S, by = "S") |>
    dplyr::select(-S) |>
    dplyr::rename(gradient_pos_density_nonfitted = gradient_pos_density) |>
    tibble::as_tibble() |>
    dplyr::mutate(gradient_pos_density = dplyr::case_when(
      .cooksd >= COOKS_CUTOFF ~ .fitted,
      TRUE ~ gradient_pos_density_nonfitted
    )) |>
    dplyr::select(-.cooksd, -.fitted, -COOKS_CUTOFF) |>
    qsip_sample_data(overwrite = TRUE,
                     gradient_pos_rel_amt = "gradient_pos_rel_amt")
}
