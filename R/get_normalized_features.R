#' Get normalized features from jgi_normalize_features
#'
#' @export

get_normalized_features = function(normalized_df) {

  normalized_df |>
    tidyr::unnest(cols = EXP_predict) |>
    dplyr::select(sample_id, .fitted, feature_id) |>
    dplyr::mutate(.fitted = exp(.fitted)) |>
    tidyr::pivot_wider(values_from = .fitted, names_from = sample_id, values_fill = 0)
}
