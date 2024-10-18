#' Fit regression model to spike-in control data
#'
#' @param control_data A data frame containing the spike-in control data
#' @param engine The regression engine to use. Choices are "lm" or "glm"
#'
#' @export

fit_regression_model <- function(control_data,
                        engine = "lm") { # choices are "lm" or "glm"

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
    parsnip::set_engine(engine)

  wf <- workflows::workflow() |>
    workflows::add_model(mod)

  # create recipe
  recipe_train <-
    recipes::recipe(RATIO ~ COVERAGE,
                    data = control_data)# |>
  # step_normalize(all_predictors())

  # fit workflow on train data
  fit_wf <- wf |>
    workflows::add_recipe(recipe_train) |>
    parsnip::fit(data = control_data)

  fit_wf
}
