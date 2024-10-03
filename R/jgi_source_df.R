#' Make a source dataframe from JGI proposal file
#'
#' @export

jgi_source_df <- function(file_path, skip = 27) {
  readr::read_lines(file_path,
    skip = skip
  ) |>
    tibble::enframe() |>
    dplyr::filter(stringr::str_detect(value, "^\\d+\\. ")) |>
    tidyr::separate(value, sep = "\t", into = c("Source_sample", "Source_sample_ID", "Sample_group", "Group ID", "Isotope_label", "SIP_combined_assembly_AP_ID")) |>
    tidyr::separate(Source_sample, sep = " ", into = c("SOURCE", "SOURCE_ID")) |>
    dplyr::mutate(SOURCE = stringr::str_remove(SOURCE, "\\.")) |>
    dplyr::select(SOURCE, source_mat_id = SOURCE_ID, isotope = Isotope_label)
}
