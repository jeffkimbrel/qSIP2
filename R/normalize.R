#' Normalize feature coverages according to spike-in controls with a known ratio
#'
#' Given a dataframe of features and their coverages, this function will
#' normalize the coverages of the features according to the coverages of the
#' spike-in controls. Both the control and experimental features should be mixed
#' into the same dataframe, and the control features will be identified based on
#' internal data.
#'
#' Using the control features and their known stoichiometry, a regression model
#' is built converting the coverage to a picogram (pg) amount of DNA. This model
#' is run against the experimental data to convert their coverages into pg
#' amounts of DNA.
#'
#' This function requires the samples dataframe because it contains the MIX and
#' sequins_pg columns, which are needed to normalize the features.
#'
#' The output is a sort of intermediate file where a user can get the normalized
#' experimental data with the `get_normalized_features()` function, which is the
#' input to the main `qSIP2` workflow. Or, the user can get the normalized
#' control data with the `get_normalized_controls()` function in order to see
#' how well the model fits the spike-in controls, and to assess how well each
#' individual spike-in sequences is behaving.
#'
#' @param features A dataframe with feature_id and coverage columns
#' @param samples A dataframe with sample_id, MIX, and sequins_pg columns
#' @param method The regression engine to use. Choices are "lm" or "glm"
#'
#' @export
#'
#' @returns A tibble with nested dataframes for each sample, containing the original coverage data, the normalized coverage data, and the model information
#'
#' @family "spike-ins"


jgi_normalize_features <- function(features, samples, method = "lm") {

  # method must be either "lm" or "glm"
  #stopifnot("<method> should be either `lm` or `glm`" = method %in% c("lm", "glm"))

  # features should be a dataframe with feature_id and coverage columns
  stopifnot("<features> should be a dataframe with feature_id and coverage columns" = all(c("feature_id") %in% colnames(features)))

  # samples should be a dataframe
  stopifnot("<samples> should be a dataframe" = is.data.frame(samples))


  # make a three column nested dataframe, one for for each sample, and then nested dataframes for log coverages of the EXP and CONTROL features
  features_nested <- features %>%
    tidyr::pivot_longer(cols = -feature_id, names_to = "sample_id", values_to = "COVERAGE") |>
    dplyr::left_join(samples, by = "sample_id") |>
    dplyr::select(sample_id, feature_id, COVERAGE, MIX, sequins_pg) |>
    dplyr::mutate(TYPE = ifelse(feature_id %in% jgi_mixes$feature_id, "CONTROL", "EXP")) |> # label each feature_id for if it is in the jgi_mixes dataframe
    dplyr::filter(COVERAGE > 0) |> # needed to make the rownames the same in the prediction later, because it drops the NAs
    dplyr::mutate(COVERAGE = log(COVERAGE)) |>
    tidyr::nest(data = -c(sample_id, TYPE)) |>
    tidyr::pivot_wider(names_from = TYPE, values_from = data)

  # see slight differences between newdata and new_data -
  features_predictions <- features_nested |>
    dplyr::mutate(workflow = purrr::map(CONTROL, purrr::possibly(fit_regression_model, otherwise = NA), method = method)) |> # make a workflows::workflow
    dplyr::mutate(
      EXP_predict = purrr::map2(
        workflow,
        EXP,
        ~ broom::augment(parsnip::extract_fit_engine(.x),
            newdata = .y, # newdata uses data passed as .y
            interval = "confidence"
          )
      ),
      CONTROL_predict = purrr::map2(
        workflow,
        CONTROL,
        ~ broom::augment(parsnip::extract_fit_engine(.x),
            new_data = .y, # new_data uses data in the model, basically ignoring .y
            interval = "confidence"
          )
      ),
      model_info = purrr::map(workflow, broom::glance),
      coefficient_info = purrr::map(workflow, broom::tidy)
    )

  return(features_predictions)
}


#' Fit regression model to spike-in control data
#'
#' This is an internal function that does the actual fitting of the regression
#' model to the spike-in control data, and is called by `jgi_normalize_features()`
#' via a `purrr::map()` call after nesting the data.
#'
#' The output is a workflow object that can be used for fitting with
#' `broom::augment(parsnip::extract_fit_engine(x))`.
#'
#' @param control_data A data frame containing the spike-in control data
#' @param method The regression engine to use. Choices are "lm" or "glm"
#'
#' @keywords internal
#'
#' @family "spike-ins"
#'
#' @returns a workflow object


fit_regression_model <- function(control_data,
                                 method = "lm") { # choices are "lm" or "glm"

  COVERAGE <- feature_id <- MIX <- RATIO <- NULL

  # process coverage data (log and remove non-control features) and join with internal jgi_mixes dataframe.
  control_data <- control_data |>
    dplyr::filter(COVERAGE > -Inf) |>
    dplyr::filter(feature_id %in% jgi_mixes$feature_id) |>
    dplyr::left_join(jgi_mixes, by = dplyr::join_by(feature_id, MIX)) |>
    dplyr::select(feature_id, COVERAGE, MIX, RATIO) |>
    dplyr::mutate(RATIO = log(RATIO))

  # build regression model
  mod <-
    parsnip::linear_reg(mode = "regression") |>
    parsnip::set_engine(method)

  wf <- workflows::workflow() |>
    workflows::add_model(mod)

  # create recipe
  recipe_train <-
    recipes::recipe(RATIO ~ COVERAGE,
      data = control_data
    ) # |>
  # step_normalize(all_predictors())

  # fit workflow on train data
  fit_wf <- wf |>
    workflows::add_recipe(recipe_train) |>
    parsnip::fit(data = control_data)

  return(fit_wf)
}


#' Make a feature dataframe from a JGI coverage file
#'
#' @param coverage_file A file path to a JGI coverage file
#'
#' @export
#'
#' @family "spike-ins"
#'
#' @returns A dataframe suitable for `qSIP2` workflows

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



#' Make a sample dataframe from JGI proposal file
#'
#' @param file_path A file path to a JGI proposal file
#' @param sources A dataframe with source information
#' @param skip The number of lines to skip in the proposal file
#'
#' @export
#'
#' @family "spike-ins"
#'
#' @returns A dataframe suitable for `qSIP2` workflows

jgi_sample_df <- function(file_path, sources, skip = 27) {
  readr::read_lines(file_path,
    skip = skip
  ) |>
    tibble::enframe() |>
    dplyr::filter(stringr::str_detect(value, "^\\d+\\.\\d+")) |>
    tidyr::separate(value, sep = "\t", into = c("Gradient_position", "sample_id", "Fraction_eluted_volume (uL)", "Fraction_density (g/mL)", "Eluted_DNA_concentration (ng/uL)", "Run_date", "Library_name", "Fastq_name", "Sequencing_project_ID", "Sequins_added (pg)", "Mix_type", "Raw_reads_count", "Filtered_reads_count")) |>
    #tidyr::separate(Fraction_name, sep = " ", into = c("Fraction", "sample_id")) |>
    tidyr::separate(Gradient_position, sep = "\\.", into = c("SOURCE", "Fraction")) |>
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
#' @param file_path A file path to a JGI proposal file
#' @param skip The number of lines to skip in the proposal file
#'
#' @export
#'
#' @family "spike-ins"
#'
#' @returns A dataframe suitable for `qSIP2` workflows

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



#' Get normalized controls from jgi_normalize_features
#'
#' @param normalized_df A dataframe from jgi_normalize_features
#'
#' @export
#'
#' @family "spike-ins"
#'
#' @returns A dataframe

get_normalized_controls <- function(normalized_df) {

  sample_id <- CONTROL_predict <- CONTROL <- CONTROL_coverage <- feature_id <- MIX <- sequins_pg <- pg <- NULL

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




#' Get normalized features from jgi_normalize_features
#'
#' @param normalized_df A dataframe from jgi_normalize_features
#'
#' @export
#'
#' @family "spike-ins"
#'
#' @returns A dataframe suitable for `qSIP2` workflows

get_normalized_features = function(normalized_df) {

  normalized_df |>
    tidyr::unnest(cols = EXP_predict) |>
    dplyr::select(sample_id, .fitted, feature_id) |>
    dplyr::mutate(.fitted = exp(.fitted)) |> # undo the log transformation
    tidyr::pivot_wider(values_from = .fitted, names_from = sample_id, values_fill = 0)
}

