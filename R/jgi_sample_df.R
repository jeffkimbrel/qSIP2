#' Make a sample dataframe from JGI proposal file
#'
#' @export

jgi_sample_df <- function(file_path, skip = 27) {
  readr::read_lines(proposal_file,
    skip = skip
  ) |>
    tibble::enframe() |>
    dplyr::filter(stringr::str_detect(value, "^\\d+\\.\\d+")) |>
    tidyr::separate(value, sep = "\t", into = c("Fraction", "Fraction_eluted_volume (uL)", "Fraction_density (g/mL)", "Eluted_DNA_concentration (ng/uL)", "Run_date", "Library_name", "Fastq_name", "Sequencing_project_ID", "Sequins_added (pg)", "Mix_type", "Raw_reads_count", "Filtered_reads_count")) |>
    tidyr::separate(Fraction, sep = " ", into = c("Fraction", "sample_id")) |>
    tidyr::separate(Fraction, sep = "\\.", into = c("SOURCE", "Fraction")) |>
    dplyr::select(sample_id,
      SOURCE,
      gradient_pos = Fraction,
      gradient_pos_density = `Fraction_density (g/mL)`,
      eluted_volume_ul = `Fraction_eluted_volume (uL)`,
      eluted_conc_ng_ul = `Eluted_DNA_concentration (ng/uL)`,
      sequins_pg = `Sequins_added (pg)`,
      MIX = Mix_type
    ) |>
    dplyr::left_join(sources, by = "SOURCE") |>
    dplyr::select(-SOURCE)
}
