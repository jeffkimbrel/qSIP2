#' Get normalized features from jgi_normalize_features
#'
#' @export

get_normalized_features = function(data) {

  data |>
    tidyr::unnest(cols = pred_experimental) |>
    dplyr::select(sample_id, .pred, feature_id) |>
    dplyr::mutate(.pred = exp(.pred)) |>
    tidyr::pivot_wider(values_from = .pred, names_from = sample_id, values_fill = 0)
}
