#'
#'
#' @export


get_normalized_controls <- function(normalized_df) {

  normalized_df |>
    dplyr::select(sample_id, CONTROL_predict, CONTROL) |>
    tidyr::unnest(cols = c(CONTROL, CONTROL_predict), names_sep = "_") |>
    dplyr::select(-CONTROL_COVERAGE) |>
    dplyr::rename_all(gsub, pattern = "CONTROL_predict_", replacement = "") |>
    dplyr::rename_all(gsub, pattern = "CONTROL_", replacement = "") |>
    dplyr::left_join(jgi_mixes, by = dplyr::join_by(feature_id, MIX)) |>
    dplyr::mutate(pg = exp(.fitted) * as.numeric(sequins_pg)) |>
    dplyr::select(sample_id, feature_id, MIX, pg, dplyr::everything(), -`..y`)
}
