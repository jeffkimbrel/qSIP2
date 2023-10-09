

summarize_EAF_values = function(j, CI = 0.9) {

  message(glue::glue("CI = {CI}"))

  j@EAF %>%
    dplyr::group_by(feature_id, observed) |>
    dplyr::summarize(lower = quantile(EAF, (1-CI)/2, na.rm = T),
                     upper = quantile(EAF, 1-(1-CI)/2, na.rm = T),
                     mean_EAF = mean(EAF),
                     .groups = "drop")
}
