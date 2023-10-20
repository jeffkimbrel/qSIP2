#' Summarize the observed and resampled EAF values
#'
#' Reports observed EAF value (`observed_EAF`) as well as the mean of the resampled
#' values (`mean_resampled_EAF`) and the `lower` and `upper` confidence interval
#' with a given `confidence` limit.
#'
#' The confidence interval uses the resampling method where it returns the quantile
#' values from the resampled data. If `confidence = 0.9` (the default) then this
#' function returns the 5% and 95% quantiles (representing 90% of the resampling)
#' as the `lower` and `upper` results.
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object
#' @param confidence (*numeric, default: 0.9*) The confidence level for the confidence interval
#'
#' @export
#'
#' @returns A `dataframe` with summarized observed and resampled EAF values

summarize_EAF_values <- function(qsip_data_object, confidence = 0.9) {
  if (confidence >= 1 | confidence <= 0) {
    stop("ERROR: confidence level should be between 0 and 1")
  }

  message(glue::glue("Confidence level = {confidence}"))

  resamples <- qsip_data_object@EAF %>%
    dplyr::filter(observed == FALSE) |>
    dplyr::group_by(feature_id) |>
    dplyr::summarize(
      mean_resampled_EAF = mean(EAF),
      lower = quantile(EAF, (1 - confidence) / 2, na.rm = T),
      upper = quantile(EAF, 1 - (1 - confidence) / 2, na.rm = T),
      .groups = "drop"
    )

  observed <- qsip_data_object@EAF %>%
    dplyr::filter(observed == TRUE) |>
    dplyr::select(feature_id, observed_EAF = EAF)

  observed |>
    dplyr::left_join(resamples, by = "feature_id")
}
