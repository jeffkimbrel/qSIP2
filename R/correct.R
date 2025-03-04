#' Correct sample density values using gradient position information
#'
#' Used in conjunction with `plot_density_outliers()`, this function will correct
#' the gradient_pos_density values for outliers based on the gradient_position
#' values. A linear model is built using the gradient_position and
#' gradient_pos_density, and if the Cook's outlier value is above the cutoff, then
#' that sample's gradient_pos_density value is replaced with the fitted value. If
#' it isn't above the cut-off, then no correction is made for that sample.
#'
#' @param sample_data (*qsip_sample_data*) A qsip object with sample data
#' @param sensitivity (*numeric, default: 4*) A sensitivity value, with lower values being more sensitive to outlier detection and correction
#'
#' @export
#'
#' @returns A qsip_sample_data object with corrected gradient_pos_density values

correct_gradient_pos_density <- function(sample_data,
                                         sensitivity = 4) {
  stopifnot("sample_data should be of class <qsip_sample_data>" = inherits(sample_data, qsip_sample_data))

  # sensitivity should a positive number
  stopifnot("sensitivity should be a positive number" = is.numeric(sensitivity) & sensitivity > 0)


  data <- sample_data@data |>
    dplyr::filter(gradient_position > 0)

  S <- data |>
    dplyr::group_by(source_mat_id) |>
    dplyr::do(broom::augment(stats::lm(gradient_pos_density ~ gradient_position, data = .))) |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_"), COOKS_CUTOFF = sensitivity / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(S, .cooksd, COOKS_CUTOFF, .fitted)

  data |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_")) |>
    dplyr::left_join(S, by = "S") |>
    dplyr::select(-S) |>
    dplyr::rename(gradient_pos_density_nonfitted = gradient_pos_density) |>
    tibble::as_tibble() |>
    dplyr::mutate(gradient_pos_density = dplyr::case_when(
      .cooksd >= COOKS_CUTOFF ~ .fitted,
      TRUE ~ gradient_pos_density_nonfitted
    )) |>
    dplyr::select(-.cooksd, -.fitted, -COOKS_CUTOFF) |>
    qsip_sample_data(
      overwrite = TRUE,
      gradient_pos_rel_amt = "gradient_pos_rel_amt"
    )
}



#' Correct gradient position density (gpd) values using bootstrapped features
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object
#' @param bootstraps (*numeric, default: 1000*) The number of bootstraps to perform
#' @param fraction_cutoff (*numeric, default: 5*) The minimum number of fractions a feature must be found in to be considered
#' @param source_cutoff (*numeric, default: 3*) The minimum number of sources a feature must be found in to be considered
#' @param quiet (*logical, default: FALSE*) Whether to print a message about the number of features found
#' @param return (*character, default: "qsip_data_object") Whether to return the corrections or the qsip_data_object
#'
#' @export

correct_gpd_bootstrap <- function(qsip_data_object,
                                  bootstraps = 1000,
                                  fraction_cutoff = 5,
                                  source_cutoff = 3,
                                  quiet = F,
                                  return = "qsip_data_object") {
  # return should be either "corrections" or "qsip_data_object"
  stopifnot("<return> should be either 'corrections' or 'qsip_data_object'" = return %in% c("corrections", "qsip_data_object"))


  wad_reference <- iq_get_wad_reference(qsip_data_object,
    fraction_cutoff = fraction_cutoff,
    source_cutoff = source_cutoff,
    quiet = quiet
  )

  df_for_resampling <- ig_get_df_for_resampling(qsip_data_object,
    wad_reference,
    fraction_cutoff = fraction_cutoff,
    source_cutoff = source_cutoff
  )

  corrections <- df_for_resampling |>
    dplyr::mutate(b = purrr::map(data, ~ rsample::bootstraps(
      . |>
        dplyr::select(difference_to_mean),
      times = bootstraps
    ))) |>
    tidyr::unnest(b) |>
    dplyr::mutate(m = purrr::map_dbl(
      splits,
      function(x) {
        dat <- as.data.frame(x)$difference_to_mean
        median(dat)
      }
    )) |>
    dplyr::summarise(
      correction_mean = mean(m),
      correction_median = median(m),
      .by = source_mat_id
    ) |>
    dplyr::arrange(correction_mean)

  if (return == "corrections") {
    return(corrections)
  } else if (return == "qsip_data_object") {
    qf <- qsip_data_object@feature_data
    qm <- qsip_data_object@source_data

    qs <- get_dataframe(qsip_data_object, "sample") |>
      dplyr::left_join(corrections, by = "source_mat_id") |>
      dplyr::mutate(gradient_pos_density = gradient_pos_density + correction_median) |>
      qsip_sample_data(
        gradient_pos_rel_amt = "gradient_pos_rel_amt",
        gradient_pos_density = "gradient_pos_density",
        overwrite = T
      )

    q_corrected <- qsip_data(qm, qs, qf)
    print(corrections)
    return(q_corrected)
  }
}



#' Correct gradient position density (gpd) values using linear regression approach
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object
#' @param method (*character*) The lm method to use
#' @param fraction_cutoff (*numeric, default: 5*) The minimum number of fractions a feature must be found in to be considered
#' @param source_cutoff (*numeric, default: 3*) The minimum number of sources a feature must be found in to be considered
#' @param quiet (*logical, default: FALSE*) Whether to print a message about the number of features found
#' @param return (*character, default: "qsip_data_object") Whether to return the corrections or the qsip_data_object
#'
#' @export

correct_gpd_rlm <- function(qsip_data_object,
                            method = "siegel",
                            fraction_cutoff = 5,
                            source_cutoff = 3,
                            quiet = F,
                            return = "qsip_data_object") {
  # return should be either "corrections" or "qsip_data_object"
  stopifnot("<return> should be either 'corrections' or 'qsip_data_object'" = return %in% c("corrections", "qsip_data_object"))


  wad_reference <- iq_get_wad_reference(qsip_data_object,
    fraction_cutoff = fraction_cutoff,
    source_cutoff = source_cutoff,
    quiet = quiet
  )

  df_for_resampling <- ig_get_df_for_resampling(qsip_data_object,
    wad_reference,
    fraction_cutoff = fraction_cutoff,
    source_cutoff = source_cutoff
  )

  if (method == "siegel") {
    siegel <- qsip_data_object@wads |>
      dplyr::filter(feature_id %in% wad_reference$feature_id) |>
      dplyr::left_join(wad_reference, by = dplyr::join_by(feature_id)) |>
      dplyr::left_join(get_dataframe(qsip_data_object, type = "source"), by = dplyr::join_by(source_mat_id)) |>
      tidyr::nest(data = -source_mat_id) |>
      dplyr::mutate(siegel = purrr::map(data, ~ RobustLinearReg::siegel_regression(WAD_reference_mean ~ WAD, .x),
        progress = TRUE
      ))

    corrections <- get_dataframe(qsip_data_object, "sample") |>
      dplyr::rename(WAD = gradient_pos_density) |>
      tidyr::nest(data = -source_mat_id) |>
      dplyr::left_join(siegel, by = "source_mat_id") |>
      dplyr::mutate(p = purrr::map2(siegel, data.x, ~ broom::augment(.x, newdata = .y))) |>
      tidyr::unnest(p)

    if (return == "corrections") {
      return(corrections)
    } else if (return == "qsip_data_object") {
      qf <- qsip_data_object@feature_data
      qm <- qsip_data_object@source_data

      qs <- corrections |>
        dplyr::select(-data.x, -data.y, -siegel) |>
        qsip_sample_data(
          gradient_pos_density = ".fitted",
          overwrite = T
        )

      q <- qsip_data(qm, qs, qf)
      return(q)
    }
  }
}



#' Get unlabeled features for WAD reference (internal)
#'
#' Internal function used by the "correct_gpd_" functions to grab unlabeled features
#' that fit a certain prevalence criteria, and return their mean and median WAD
#' values, as well as their standard deviation, coefficient of variation, and number
#' of sources/fractions found in.
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object
#' @param fraction_cutoff (*numeric, default: 5*) The minimum number of fractions a feature must be found in to be considered
#' @param source_cutoff (*numeric, default: 3*) The minimum number of sources a feature must be found in to be considered
#' @param quiet (*logical, default: FALSE*) Whether to print a message about the number of features found
#'
#' @keywords internal
#'
#' @returns A tibble

iq_get_wad_reference <- function(qsip_data_object,
                                 fraction_cutoff = 5,
                                 source_cutoff = 3,
                                 quiet = F) {
  # error if is_qsip_data(qsip_data_object) is not true
  if (!inherits(qsip_data_object, qsip_data)) {
    stop("<qsip_data_object> must be of class qsip_data", call. = FALSE)
  }

  # fraction_cutoff must be an integer greater than 0
  if (!is.numeric(fraction_cutoff) | fraction_cutoff < 1) {
    stop("<fraction_cutoff> must be an integer greater than 0", call. = FALSE)
  }

  # source_cutoff must be an integer greater than 0
  if (!is.numeric(source_cutoff) | source_cutoff < 1) {
    stop("<source_cutoff> must be an integer greater than 0", call. = FALSE)
  }

  wad_reference <- qsip_data_object@wads |>
    dplyr::filter(n_fractions >= fraction_cutoff) |>
    dplyr::left_join(get_dataframe(qsip_data_object, "source"), by = "source_mat_id") |>
    dplyr::filter(source_mat_id %in% get_all_by_isotope(qsip_data_object, "unlabeled", quiet = T)) |>
    dplyr::mutate(n_sources = dplyr::n(), .by = "feature_id") |>
    dplyr::filter(n_sources >= source_cutoff) |>
    dplyr::summarize(
      WAD_reference_mean = mean(WAD),
      sd = sd(WAD),
      cov = sd / WAD_reference_mean,
      WAD_reference_median = median(WAD),
      n_sources = dplyr::n(),
      mean_fractions = sum(n_fractions) / n_sources,
      .by = feature_id
    ) |>
    dplyr::arrange(feature_id)

  if (!quiet) {
    message(wad_reference |>
      dplyr::n_distinct("feature_id"), " reference features found with at least ", fraction_cutoff, " fractions and ", source_cutoff, " sources")
  }

  return(wad_reference)
}



#' Make nested dataframe for gpd adjustment sampling
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object
#' @param wad_reference (*tibble*) A tibble of WAD reference values
#' @param fraction_cutoff (*numeric, default: 5*) The minimum number of fractions a feature must be found in to be considered
#' @param source_cutoff (*numeric, default: 3*) The minimum number of sources a feature must be found in to be considered
#'
#' @keywords internal

ig_get_df_for_resampling <- function(qsip_data_object,
                                     wad_reference,
                                     fraction_cutoff = 5,
                                     source_cutoff = 3) {
  df_for_resampling <- qsip_data_object@wads |>
    dplyr::filter(feature_id %in% wad_reference$feature_id) |>
    dplyr::filter(n_fractions >= fraction_cutoff) |>
    dplyr::select(feature_id, source_mat_id, WAD, n_fractions) |>
    dplyr::mutate(n_sources = dplyr::n(), .by = "feature_id") |>
    dplyr::filter(n_sources >= source_cutoff) |>
    dplyr::left_join(wad_reference, by = "feature_id") |>
    dplyr::mutate(
      difference_to_mean = (WAD_reference_mean - WAD), # positive means it is less dense than the mean
      stress_to_mean = difference_to_mean^2
    ) |>
    tidyr::nest(data = -source_mat_id) |>
    dplyr::arrange(source_mat_id)

  return(df_for_resampling)
}

#' Plot difference between feature WAD and WAD reference
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object
#' @param fraction_cutoff (*numeric, default: 5*) The minimum number of fractions a feature must be found in to be considered
#' @param source_cutoff (*numeric, default: 3*) The minimum number of sources a feature must be found in to be considered
#' @param quiet (*logical, default: FALSE*) Whether to print a message about the number of features found
#'
#' @export

plot_difference_to_mean <- function(qsip_data_object,
                                    fraction_cutoff = 5,
                                    source_cutoff = 3,
                                    quiet = F) {

  wad_reference <- iq_get_wad_reference(qsip_data_object,
    fraction_cutoff = fraction_cutoff,
    source_cutoff = source_cutoff,
    quiet = quiet
  )

  qsip_data_object@wads |>
    dplyr::left_join(wad_reference, by = "feature_id") |>
    dplyr::filter(!is.na(WAD_reference_mean)) |>
    dplyr::left_join(get_dataframe(qsip_data_object, type = "source"), by = dplyr::join_by(source_mat_id)) |>
    ggplot2::ggplot(ggplot2::aes(y = source_mat_id, x = WAD - WAD_reference_mean, fill = factor(ggplot2::after_stat(quantile)))) +
    ggridges::stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = 4,
      # quantile_lines = TRUE,
      bandwidth = 5e-4
    ) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "WAD minus Reference WAD", fill = "quantiles") +
    ggplot2::facet_wrap(~isotope, scales = "free_y") +
    ggplot2::geom_vline(xintercept = 0, linetype = 3, color = "purple", linewidth = 1)
}







#' Plot curves of corrected sample curves versus reference
#'
#' @param reference (*qsip_data*) A qsip data object
#' @param corrected (*qsip_data*) A qsip data object
#'
#' @export

plot_corrected_curves <- function(reference, corrected) {
  l <- list(
    "reference" = dplyr::left_join(slot(reference, "wads"), slot(reference, "tube_rel_abundance"), by = dplyr::join_by(feature_id, source_mat_id)),
    "corrected" = dplyr::left_join(slot(corrected, "wads"), slot(corrected, "tube_rel_abundance"), by = dplyr::join_by(feature_id, source_mat_id))
  )

  dplyr::bind_rows(l, .id = "correction_method") |>
    dplyr::summarize(tube_rel_abundance = sum(tube_rel_abundance), .by = c(correction_method, source_mat_id, gradient_pos_density, sample_id)) |>
    dplyr::left_join(get_dataframe(reference, type = "source"), by = dplyr::join_by(source_mat_id)) |>
    ggplot2::ggplot(ggplot2::aes(x = gradient_pos_density, y = correction_method)) +
    ggridges::geom_density_ridges(ggplot2::aes(fill = correction_method, height = tube_rel_abundance), stat = "identity", alpha = 0.9) +
    ggplot2::facet_wrap(~ isotope + source_mat_id, ncol = 6) +
    ggplot2::scale_fill_manual(values = c("reference" = "gray70", "corrected" = "dodgerblue")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
}


#' Plot the correction statistics
#'
#' @param reference (*qsip_data*) A qsip data object
#' @param corrected (*qsip_data*) A qsip data object
#' @param source_cutoff (*numeric, default: 3*) The minimum number of sources a feature must be found in to be considered
#' @param fraction_cutoff (*numeric, default: 5*) The minimum number of fractions a feature must be found in to be considered
#' @param quiet (*logical, default: FALSE*) Whether to print a message about the number of features found
#'
#' @export

plot_correction_stats <- function(reference, corrected, source_cutoff = 3, fraction_cutoff = 5, quiet = F) {

  l <- list(
    "reference" = reference@wads,
    "corrected" = corrected@wads
  )

  wad_reference <- iq_get_wad_reference(reference, source_cutoff = source_cutoff, fraction_cutoff = fraction_cutoff, quiet = quiet)

  dplyr::bind_rows(l, .id = "correction_method") |>
    dplyr::mutate(correction_method = forcats::fct_relevel(correction_method, c("reference", "corrected"))) |>
    dplyr::left_join(wad_reference, by = dplyr::join_by(feature_id)) |>
    dplyr::left_join(get_dataframe(reference, type = "source"), by = dplyr::join_by(source_mat_id)) |>
    dplyr::group_by(correction_method, source_mat_id, isotope) |>
    yardstick::metrics(truth = WAD_reference_mean, estimate = WAD) |>
    ggplot2::ggplot(ggplot2::aes(x = correction_method, y = .estimate, color = isotope)) +
    ggplot2::geom_boxplot(fill = NA) +
    ggplot2::geom_point(position = ggplot2::position_dodge2(width = 0.8)) +
    ggplot2::facet_wrap(~.metric, scales = "free_y", ncol = 3) +
    ggplot2::scale_color_manual(values = isotope_palette) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::expand_limits(y = 0)
}


#' Get compression values from a rlm correction
#'
#' @param corrections (*tibble*) A tibble of corrections from `correct_gpd_rlm()`
#'
#' @export

get_correction_compression = function(corrections) {
  corrections |>
    dplyr::summarize(compression = (max(.fitted) - min(.fitted)) / (max(WAD) - min(WAD)), .by = source_mat_id)
}

#' Plot correction compression
#'
#' @export

plot_correction_compression = function(corrections) {

  correction_values = get_correction_compression(corrections)

  corrections |>
    dplyr::left_join(correction_values, by = "source_mat_id") |>
    ggplot2::ggplot(ggplot2::aes(x = WAD, y = .fitted)) +
    ggplot2::geom_point(aes(color = compression)) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::facet_wrap(~source_mat_id) +
    geom_abline(intercept = 0, slope = 1, linetype = 3) +
    labs(x = "original WAD", y = "corrected WAD")
}
