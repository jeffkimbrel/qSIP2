#' test
#'
#' @export

correct_gradient_pos_density <- function(qsip_sample_data,
                                         sensitivity = 4) {

  data = qsip_sample_data@data |>
    dplyr::filter(gradient_position > 0)

  S <- data |>
    dplyr::group_by(source_mat_id) |>
    dplyr::do(broom::augment(stats::lm(gradient_pos_density ~ gradient_position, data = .))) |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_"), COOKS_CUTOFF = sensitivity / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(S, .fitted)

  data |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_")) |>
    dplyr::left_join(S, by = "S") |>
    dplyr::select(-S) |>
    dplyr::rename(gradient_pos_density_nonfitted = gradient_pos_density) |>
    tibble::as_tibble()

}
