#' asdf
#'
#' @export


jgi_normalize_features <- function(features, method = "lm") {
  j <- features %>%
    tidyr::pivot_longer(cols = -feature_id, names_to = "sample_id", values_to = "COVERAGE") |>
    dplyr::left_join(samples, by = "sample_id") |>
    dplyr::select(sample_id, feature_id, COVERAGE, MIX, sequins_pg) |>
    dplyr::mutate(TYPE = ifelse(feature_id %in% jgi_mixes$feature_id, "CONTROL", "EXP")) |>
    dplyr::filter(COVERAGE > 0) |> # needed to make the rownames the same in the prediction later, because it drops the NAs
    dplyr::mutate(COVERAGE = log(COVERAGE)) |> # log all data before hand
    tidyr::nest(data = -c(sample_id, TYPE)) |>
    tidyr::pivot_wider(names_from = TYPE, values_from = data)

  # features = j[1,]$CONTROL[[1]] |> pull(COVERAGE)

  j2 <- j |>
    dplyr::mutate(workflow = purrr::map(CONTROL, purrr::possibly(fit_regression_model, otherwise = NA), engine = method)) |>
    dplyr::mutate(
      EXP_predict = purrr::map2(workflow, EXP, ~ broom::augment(parsnip::extract_fit_engine(.x),
                                                                          newdata = .y, # newdata uses data passed as .y
                                                                          interval = "confidence")),
      CONTROL_predict = purrr::map2(workflow, CONTROL, ~ broom::augment(parsnip::extract_fit_engine(.x),
                                                                         new_data = .y,  # new_data uses data in the model, basically ignoring .y
                                                                         interval = "confidence")),
      model_info = purrr::map(workflow, broom::glance),
      coefficient_info = purrr::map(workflow, broom::tidy)
    )

  return(j2)
}
