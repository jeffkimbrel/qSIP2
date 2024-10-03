#' Make a feature dataframe from a JGI coverage file
#'
#' @export

jgi_feature_df <- function(coverage_file) {
  readr::read_csv(coverage_file, show_col_types = F) |>
    tidyr::pivot_longer(
      cols = c(dplyr::everything(), -Feature),
      names_to = "sample_id",
      values_to = "COVERAGE"
    ) |>
    dplyr::rename("feature_id" = "Feature")
}
