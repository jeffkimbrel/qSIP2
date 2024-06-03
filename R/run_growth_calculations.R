#' Calculate total abundances at timepoint t
#'
#' This function takes a `qsip_data` object and calculates the total abundance of
#' each feature at time zero. This should be done on an early `qsip_data` object
#' that still has time zero data.
#'
#' Sometimes, a feature will have abundance at a later time point, but no values
#' for time zero. If a feature has zero abundance at time zero, a warning will
#' be issued, but the feature will still be included in the output with a starting
#' abundance of zero.
#'
#' @param qsip_data_object (*qsip_data*) An object of `qsip_data` class
#' @param timepoint (*character*) The name of the timepoint column in the source data
#' @param t (*numeric*) The value of the timepoint column to filter on
#' @param group (*character*) The name of the grouping variable(s) to summarize the counts
#'
#' @export
#'
#' @returns (*data.frame*) A data frame with feature_id and total abundance at time zero

get_N_total_it <- function(qsip_data_object,
                           timepoint = "timepoint",
                           t = 0,
                           group = NULL) {
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  }

  # this section makes the basic dataframe with total abundance at time zero.
  # Because it is based on @tube_rel_abundance it will not have data for
  # features with zero abundance at time zero
  N_total_i0 <- qsip_data_object@tube_rel_abundance |>
    dplyr::summarize(REL = sum(tube_rel_abundance), .by = c(feature_id, source_mat_id)) |>
    dplyr::left_join(qsip_data_object@source_data@data, by = "source_mat_id") |>
    dplyr::select(
      timepoint = all_of(timepoint),
      dplyr::everything()
    ) |>
    dplyr::filter(timepoint == t) |>
    dplyr::mutate(N_total_i0 = REL * total_abundance)
    # dplyr::select(feature_id, source_mat_id, timepoint, N_total_i0) |>
    # TODO line below: should it be mean or sum?
    # TODO should this be filtered to only include unlabeled? Timepoints other than 0 might have labeled samples

  # if using a grouping variable(s) to summarize the counts
  # !!! : https://stackoverflow.com/questions/42612417/how-to-pass-multiple-column-names-as-input-to-group-by-in-dplyr/42612631
  # sym : https://stackoverflow.com/questions/61180201/triple-exclamation-marks-on-r
  if (isFALSE(is.null(group))) {
    N_total_i0 = N_total_i0 |>
      dplyr::group_by(feature_id, !!!dplyr::syms(group)) |>
      dplyr::summarize(N_total_i0 = mean(N_total_i0), .groups = "drop")
  } else {
    N_total_i0 = N_total_i0 |>
      dplyr::summarize(N_total_i0 = mean(N_total_i0), .by = feature_id)
  }

  # make dataframe with zeroes. This ensures feature_ids with zero abundance
  # will still be present in the resulting dataframe
  N_total_i0 <- get_dataframe(qsip_data_object, type = "feature") |>
    dplyr::select(feature_id) |>
    dplyr::mutate(zero = 0) |>
    dplyr::left_join(N_total_i0, by = "feature_id") |>
    dplyr::group_by(feature_id, !!!dplyr::syms(group)) |>
    dplyr::mutate(N_total_i0 = sum(zero, N_total_i0, na.rm = T)) |>
    dplyr::select(-zero) |>
    dplyr::ungroup()

  # report on zero abundance samples
  no_abundance <- N_total_i0 |>
    dplyr::filter(N_total_i0 == 0)

  if (nrow(no_abundance) > 0) {
    warning(glue::glue("{nrow(no_abundance)} feature_ids have zero abundance at time {t}:"), call. = F)
    warning(paste(no_abundance$feature_id, collapse = ", "), call. = F)
  }


  # add timepoint as timepoint1
  N_total_i0 <- N_total_i0 |>
    dplyr::mutate(timepoint1 = t)

  return(N_total_i0)
}


#' Run growth calculations
#'
#' @param qsip_data_object (*qsip_data*) An object of `qsip_data` class
#' @param N_total_it (*data.frame*) A data frame of time zero totals from `get_N_total_it()`
#' @param timepoint (*character*) The name of the timepoint column in the source data
#' @param growth_model (*character, default: exponential*) The growth model to use. Must be either "exponential" or "linear"
#' @param correct_copy_numbers (*character, default: filter*) If copy numbers are not logical (e.g. < 0), should they be filtered out or adjusted to 0?
#' @param correct_EAF (*character, default: filter*) If EAF values are not logical (e.g. <0 or >1), should they be filtered out or adjusted to 0 or 1?
#'
#' @export
#'

run_growth_calculations <- function(qsip_data_object,
                                    N_total_it,
                                    growth_model = "exponential",
                                    timepoint = "timepoint",
                                    correct_copy_numbers = "filter",
                                    correct_EAF = "filter") {
  # TODO validate arguments
  # TODO make @timepoint and @total_abundances are not null in @source_data@data. If
  # so, give error saying they must be declared in source_data to get growth

  is_qsip_data(qsip_data_object, error = TRUE)

  # growth_model must be either "exponential" or "linear"
  if (!growth_model %in% c("exponential", "linear")) {
    stop(glue::glue("growth_model must be either 'exponential' or 'linear', not {growth_model}"), call. = FALSE)
  }

  if (!correct_copy_numbers %in% c("filter", "adjust")) {
    stop(glue::glue("correct_copy_numbers must be either 'filter' or 'adjust', not {correct_copy_numbers}"), call. = FALSE)
  }

  if (!correct_EAF %in% c("filter", "adjust")) {
    stop(glue::glue("correct_EAF must be either 'filter' or 'adjust', not {correct_EAF}"), call. = FALSE)
  }

  # error if there is no timepoint column
  if (!timepoint %in% colnames(qsip_data_object@source_data@data)) {
    stop(glue::glue("timepoint column {timepoint} not found in source_data@data"), call. = FALSE)
  }


  time_i_totals <- qsip_data_object@tube_rel_abundance |>
    dplyr::summarize(REL = sum(tube_rel_abundance), .by = c(feature_id, source_mat_id)) |>
    dplyr::left_join(qsip_data_object@source_data@data, by = "source_mat_id") |>
    dplyr::select(
      timepoint = all_of(timepoint),
      dplyr::everything()
    ) |>
    dplyr::mutate(normalized_copies = REL * total_abundance) |>
    dplyr::select(feature_id, source_mat_id, timepoint, normalized_copies) |>
    dplyr::filter(source_mat_id %in% qsip_data_object@filter_results$labeled_source_mat_ids) |>
    # TODO line below: should it be mean or sum? They do lead to slightly different results
    dplyr::summarize(
      N_total_it = mean(normalized_copies),
      # timepoint = unique(timepoint), # weird to use unique(), other options?
      .by = c(feature_id, timepoint)
    )



  # merge timepoint i totals with timepoint 0 totals
  normalized_copies <- N_total_it |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features) |>
    dplyr::left_join(time_i_totals, by = "feature_id") |>
    dplyr::mutate(N_total_it = ifelse(is.na(N_total_it), 0, N_total_it))

  # get EAF values for timepoint i
  EAFs <- qsip_data_object@EAF |>
    dplyr::mutate(resample = ifelse(is.na(resample), 0, resample))


  # warn feature_id in normalized_copies not found in EAFs
  no_EAF_values <- normalized_copies |>
    dplyr::filter(!feature_id %in% EAFs$feature_id)

  if (nrow(no_EAF_values) > 0) {
    warning(glue::glue("{nrow(no_EAF_values)} feature_ids have zero abundance at this timepoint and have no EAF values. These values have been filtered out and added to @growth$no_EAF_values"), call. = FALSE)
    qsip_data_object@growth$no_EAF_values <- no_EAF_values
  }

  # merge copy number with EAF values
  rbd <- normalized_copies |>
    dplyr::left_join(EAFs, by = "feature_id") |>
    dplyr::mutate(M_heavy = calculate_M_heavy(propO = qsip_data_object@growth$propO, M)) |>
    dplyr::mutate(time_diff = timepoint - timepoint1) |>
    # overwrite the unlabeled copies with the EAF values
    dplyr::mutate(N_light_it = calculate_N_light_it(N_total_it, M_heavy, M_labeled, M)) |>
    # recalculate N_total_it or labeled using new unlabeled?
    # dplyr::mutate(N_total_it = labeled + unlabeled) |>
    dplyr::mutate(N_heavy_it = N_total_it - N_light_it)

  negative_unlabeled <- rbd |>
    dplyr::filter(N_light_it <= 0 | EAF > 1)

  negative_labeled <- rbd |>
    dplyr::filter(N_heavy_it <= 0 | EAF < 0)

  if (nrow(negative_unlabeled) > 0) {
    warning(glue::glue("{nrow(negative_unlabeled)} calculated values of unlabeled samples are negative. These values have been filtered out and added to @growth$negative_unlabeled"), call. = FALSE)
    qsip_data_object@growth$negative_unlabeled <- negative_unlabeled
  }

  if (nrow(negative_labeled) > 0) {
    warning(glue::glue("{nrow(negative_labeled)} calculated values of labeled samples are negative. These values have been filtered out and added to @growth$negative_labeled"), call. = FALSE)
    qsip_data_object@growth$negative_labeled <- negative_labeled
  }

  if (correct_copy_numbers == "adjust") {
    rbd <- rbd |>
      dplyr::mutate(N_light_it = dplyr::case_when(
        N_light_it < 0 ~ 0,
        TRUE ~ N_light_it
      )) |>
      dplyr::mutate(N_heavy_it = dplyr::case_when(
        N_heavy_it < 0 ~ 0,
        TRUE ~ N_heavy_it
      ))
  }

  if (correct_EAF == "adjust") {
    rbd <- rbd |>
      dplyr::mutate(EAF = dplyr::case_when(
        EAF < 0 ~ 0,
        EAF > 1 ~ 1,
        TRUE ~ EAF
      ))
  }

  # then, working with just the unlabeled that are greater than 0 and have a valid EAF
  rbd <- rbd |>
    dplyr::filter(N_light_it >= 0) |>
    dplyr::filter(N_heavy_it >= 0) |>
    dplyr::filter(EAF >= 0 & EAF <= 1) |>
    dplyr::mutate(
      di = calculate_di(N_light_it, N_total_i0, timepoint, timepoint1, growth_model = growth_model),
      bi = calculate_bi(N_total_it, N_light_it, timepoint, timepoint1, growth_model = growth_model),
      ri = di + bi,
      r_net = N_total_it - N_total_i0
    ) |>
    dplyr::select(feature_id, timepoint1, timepoint2 = timepoint, resample, N_total_i0, N_total_it, N_light_it, N_heavy_it, EAF, r_net, bi, di, ri)

  # mark observed and resamples similar to other qSIP2 objects
  rbd <- rbd |>
    dplyr::mutate(observed = ifelse(resample == 0, TRUE, FALSE)) |>
    dplyr::mutate(resample = ifelse(resample == 0, NA, resample))


  qsip_data_object@growth$rates <- rbd

  return(qsip_data_object)
}






#' Summarize growth values
#'
#' @param qsip_data_object A qsip_data object
#' @param confidence (*numeric, default: 0.9*) The confidence level for the growth values
#' @param quiet (*logical, default: FALSE*) Suppress messages
#'
#' @export

summarize_growth_values <- function(qsip_data_object, confidence = 0.9, quiet = FALSE) {
  # confirm the data is the correct type
  stopifnot("ERROR: qsip_data_object must be of type qsip_data" = "qsip_data" %in% class(qsip_data_object))

  # confirm the confidence value is numeric and between 0-1
  stopifnot("ERROR: confidence should be numeric" = is.numeric(confidence))
  if (confidence >= 1 | confidence <= 0) {
    stop("ERROR: confidence level should be between 0 and 1")
  }

  # confirm the qsip object has @growth$rates values
  stopifnot("ERROR: @growth slot is empty, have you run run_growth_calculations()?" = length(qsip_data_object@growth) > 0)

  if (length(qsip_data_object@growth) == 0 | is.null(qsip_data_object@growth$rates)) {
    stop("ERROR: @growth$rates slot is empty, have you run run_growth_calculations()?", .call = FALSE)
  }




  if (isFALSE(quiet)) {
    message(glue::glue("Confidence level = {confidence}"))
  }

  rbd_observed <- qsip_data_object@growth$rates |>
    dplyr::filter(observed == TRUE) |>
    dplyr::select(feature_id,
      timepoint1,
      timepoint2,
      N_total_i0,
      N_total_it,
      r_net,
      observed_bi = bi,
      observed_di = di,
      observed_ri = ri
    )

  rbd_resamples <- qsip_data_object@growth$rates |>
    dplyr::filter(observed == FALSE) |>
    dplyr::select(-observed) |>
    dplyr::summarize(
      resampled_N_mean = mean(N_total_it, na.rm = TRUE),
      resampled_N_sd = sd(N_total_it, na.rm = TRUE),
      resampled_N_lower = quantile(N_total_it, (1 - confidence) / 2, na.rm = T),
      resampled_N_upper = quantile(N_total_it, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_rnet_mean = mean(r_net, na.rm = TRUE),
      resampled_rnet_sd = sd(r_net, na.rm = TRUE),
      resampled_rnet_lower = quantile(r_net, (1 - confidence) / 2, na.rm = T),
      resampled_rnet_upper = quantile(r_net, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_bi_mean = mean(bi, na.rm = TRUE),
      resampled_bi_sd = sd(bi, na.rm = TRUE),
      resampled_bi_lower = quantile(bi, (1 - confidence) / 2, na.rm = T),
      resampled_bi_upper = quantile(bi, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_di_mean = mean(di, na.rm = TRUE),
      resampled_di_sd = sd(di, na.rm = TRUE),
      resampled_di_lower = quantile(di, (1 - confidence) / 2, na.rm = T),
      resampled_di_upper = quantile(di, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_ri_mean = mean(ri, na.rm = TRUE),
      resampled_ri_sd = sd(ri, na.rm = TRUE),
      resampled_ri_lower = quantile(ri, (1 - confidence) / 2, na.rm = T),
      resampled_ri_upper = quantile(ri, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_EAF_mean = mean(EAF, na.rm = TRUE),
      resampled_EAF_sd = sd(EAF, na.rm = TRUE),
      resampled_EAF_lower = quantile(EAF, (1 - confidence) / 2, na.rm = T),
      resampled_EAF_upper = quantile(EAF, 1 - (1 - confidence) / 2, na.rm = T),
      .by = feature_id
    )


  rbd <- rbd_observed |>
    dplyr::left_join(rbd_resamples, by = "feature_id") |>
    dplyr::arrange(desc(observed_ri))

  # TODO make some sort of note about infinite values

  return(rbd)
}





#' Plot growth values
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object
#' @param confidence (*numeric*) The confidence level for the confidence interval
#' @param top (*numeric*) The number of top features to plot. Use `Inf` for all
#' @param error (*character*) The type of error bars to plot. Options are 'none', 'bar', 'ribbon'
#' @param alpha (*numeric*) The transparency of the error bar/ribbon
#' @param type (*character*) The type of growth values to plot. Options are 'rates' or "copies
#'
#' @export

plot_growth_values <- function(qsip_data_object,
                               confidence = 0.9,
                               top = Inf,
                               error = "none",
                               alpha = 0.4,
                               type = "rates") {
  rbd <- summarize_growth_values(qsip_data_object, confidence = confidence)

  palette <- c(
    "ri" = "cornflowerblue",
    "bi" = "seagreen4",
    "di" = "tomato"
  )

  # rates
  rbd_rate <- rbd |>
    dplyr::slice_max(observed_ri, n = top) |>
    dplyr::mutate(feature_id = forcats::fct_reorder(feature_id, resampled_ri_mean)) |>
    tidyr::pivot_longer(
      cols = c(everything(), -feature_id, -timepoint1, -timepoint2, -N_total_i0, -N_total_it, -r_net),
      names_to = "rate",
      values_to = "value"
    ) |>
    tidyr::separate(rate,
      into = c("observed", "rate", "stat"),
      sep = "_",
      fill = "right"
    ) |>
    dplyr::filter(rate %in% c("ri", "bi", "di")) |>
    dplyr::mutate(stat = ifelse(is.na(stat), "observed", stat)) |>
    dplyr::filter(stat != "sd") |>
    dplyr::select(-observed) |>
    tidyr::pivot_wider(names_from = "stat", values_from = "value") |>
    dplyr::select(-observed)

  p_rate <- rbd_rate |>
    ggplot2::ggplot(ggplot2::aes(y = feature_id, x = mean, color = rate)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette)

  if (error == "bar") {
    p_rate <- p_rate + ggplot2::geom_errorbar(ggplot2::aes(xmin = lower, xmax = upper),
      width = 0.2,
      alpha = alpha
    )
  } else if (error == "ribbon") {
    p_rate <- p_rate +
      ggplot2::geom_ribbon(ggplot2::aes(xmin = lower, xmax = upper, group = rate, fill = rate), alpha = alpha)
  }


  # N copies
  p_N <- rbd |>
    dplyr::slice_max(N_total_it, n = top) |>
    dplyr::mutate(feature_id = forcats::fct_reorder(feature_id, N_total_it)) |>
    dplyr::select(feature_id, N_total_i0, N_total_it, r_net, resampled_N_mean, resampled_N_lower, resampled_N_upper) |>
    ggplot2::ggplot(ggplot2::aes(y = feature_id, x = (N_total_i0))) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = (resampled_N_lower), xmax = (resampled_N_upper)),
      width = 0.5,
      linewidth = 1,
      color = "orangered3",
      alpha = alpha
    ) +
    ggplot2::geom_segment(ggplot2::aes(x = N_total_i0, xend = N_total_it),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
      color = "gray50"
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(x = resampled_N_mean), color = "orangered3") +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_x_continuous(
      labels = function(x) {
        format(x,
          big.mark = ",",
          decimal.mark = ".",
          scientific = FALSE
        )
      },
      trans = "log2"
    )


  if (type == "rates") {
    p_rate
  } else if (type == "copies") {
    p_N
  } else {
    stop("type must be either 'rates' or 'copies'")
  }
}





#' Calculate M_heavy
#'
#' This is equation 4 from Koch, 2018
#'
#' @param propO (*numeric*) The proportion of oxygen coming from 18H2O versus other sources
#' @param M (*numeric*) The mass of the molecule
#'
#' @export

calculate_M_heavy <- function(propO, M) {
  M_heavy <- (12.07747 * propO) + M

  return(M_heavy)
}



#' Calculate N_Light_it
#'
#' This is equation 3 from Koch, 2018
#'
#' @param N_total_it The copy number of feature i at timepoint t
#' @param M_heavy The theoretical molecular weight of 100% labeled sequence
#' @param M_labeled The molecular weight of the labeled sequence
#' @param M The molecular weight of the unlabeled sequence
#'
#' @export

calculate_N_light_it <- function(N_total_it, M_heavy, M_labeled, M) {
  N_Light_it <- N_total_it * ((M_heavy - M_labeled) / (M_heavy - M))

  return(N_Light_it)
}



#' Calculate death rate
#'
#' Equation 6 from Koch, 2018
#'
#' @param N_light_it The unlabeled copy number of feature i at timepoint t
#' @param N_total_i0 The copy number of feature i at timepoint 0
#' @param timepoint The timepoint at which the copy number is being measured
#' @param timepoint1 The timepoint that is being compared against
#' @param growth_model (*character, default: exponential*) The growth model to use. Must be either "exponential" or "linear"
#'
#' @export


calculate_di <- function(N_light_it,
                         N_total_i0,
                         timepoint,
                         timepoint1,
                         growth_model = "exponential") {
  if (!growth_model %in% c("exponential", "linear")) {
    stop(glue::glue("growth_model must be either 'exponential' or 'linear', not {growth_model}"), call. = FALSE)
  }

  if (growth_model == "exponential") {
    di <- log(N_light_it / N_total_i0) * (1 / (timepoint - timepoint1))
  } else if (growth_model == "linear") {
    di <- (N_light_it - N_total_i0) / (timepoint - timepoint1)
  }

  return(di)
}



#' Calculate birth rate
#'
#' Equation 7 from Koch, 2018
#'
#' @param N_total_it The copy number of feature i at timepoint t
#' @param N_light_it The copy number of feature i at timepoint 0 (or timepoint that is being compared against)
#' @param timepoint The timepoint at which the copy number is being measured
#' @param timepoint1 The timepoint that is being compared against
#' @param growth_model (*character, default: exponential*) The growth model to use. Must be either "exponential" or "linear"
#'
#' @export

calculate_bi <- function(N_total_it,
                         N_light_it,
                         timepoint,
                         timepoint1,
                         growth_model = "exponential") {
  if (!growth_model %in% c("exponential", "linear")) {
    stop(glue::glue("growth_model must be either 'exponential' or 'linear', not {growth_model}"), call. = FALSE)
  }

  if (growth_model == "exponential") {
    bi <- log(N_total_it / N_light_it) * (1 / (timepoint - timepoint1))
  } else if (growth_model == "linear") {
    bi <- (N_total_it - N_light_it) / (timepoint - timepoint1)
  }

  return(bi)
}
