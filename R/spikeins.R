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
    dplyr::rename("feature_id" = "Feature") |>
    tidyr::pivot_wider(values_from = COVERAGE, names_from = sample_id, values_fill = 0)
}


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


#' Make a sample dataframe from JGI proposal file
#'
#' @export

jgi_sample_df <- function(file_path, skip = 27) {

  readr::read_lines(file_path,
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
