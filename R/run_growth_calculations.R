#' Calculate time zero feature total abundances
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
#' @param value (*numeric*) The value of the timepoint column to filter on
#'
#' @export
#'
#' @returns (*data.frame*) A data frame with feature_id and total abundance at time zero

calculate_time_zero_abundance <- function(qsip_data_object,
                                          timepoint = "timepoint",
                                          value = 0) {
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
    dplyr::filter(timepoint == value) |>
    dplyr::mutate(N_total_i0 = REL * total_abundance) |>
    # dplyr::select(feature_id, source_mat_id, timepoint, N_total_i0) |>
    # TODO line below: should it be mean or sum?
    dplyr::summarize(N_total_i0 = mean(N_total_i0), .by = feature_id)

  # make dataframe with zeroes. This ensures feature_ids with zero abundance
  # will still be present in the resulting dataframe
  N_total_i0 <- get_dataframe(qsip_data_object, type = "feature") |>
    dplyr::select(feature_id) |>
    dplyr::mutate(zero = 0) |>
    dplyr::left_join(N_total_i0, by = "feature_id") |>
    dplyr::mutate(N_total_i0 = sum(zero, N_total_i0, na.rm = T), .by = "feature_id") |>
    dplyr::select(-zero)

  # report on zero abundance samples
  no_abundance <- N_total_i0 |>
    dplyr::filter(N_total_i0 == 0)

  if (nrow(no_abundance) > 0) {
    warning("The following feature_ids have zero abundance at time zero: ", paste(no_abundance$feature_id, collapse = ", "), call. = F)
  }


  # add timepoint as timepoint1
  N_total_i0 <- N_total_i0 |>
    dplyr::mutate(timepoint1 = value)

  return(N_total_i0)
}


#' Run growth calculations
#'
#' @param qsip_data_object (*qsip_data*) An object of `qsip_data` class
#' @param time_zero_totals (*data.frame*) A data frame of time zero totals from `calculate_time_zero_totals()`
#'
#' @export
#'

run_growth_calculations <- function(qsip_data_object,
                                    time_zero_totals,
                                    timepoint = "timepoint") {
  # TODO validate arguments
  # TODO make @timepoint and @total_abundances are not null in @source_data@data. If
  # so, give error saying they must be declared in source_data to get growth


  # normalized_copies will be a table with 5 columns
  # 1. feature_id
  # 2. timepoint
  # 3. unlabeled: the number of copies of a feature without incorporation
  # 4. labeled: the number of copies of a feature with incorporation
  # 5. N_total_it: the sum of the labeled and unlabeled to get total features at time t
  # 6. N_total_i0: the total (unlabeled) features at time zero

  time_i_totals <- qsip_data_object@tube_rel_abundance |>
    dplyr::summarize(REL = sum(tube_rel_abundance), .by = c(feature_id, source_mat_id)) |>
    dplyr::left_join(qsip_data_object@source_data@data, by = "source_mat_id") |>
    dplyr::select(
      timepoint = all_of(timepoint),
      dplyr::everything()
    ) |>
    dplyr::mutate(normalized_copies = REL * total_abundance) |>
    dplyr::select(feature_id, source_mat_id, timepoint, normalized_copies) |>
    dplyr::mutate(type = dplyr::case_when(
      source_mat_id %in% qsip_data_object@filter_results$unlabeled_source_mat_ids ~ "unlabeled",
      source_mat_id %in% qsip_data_object@filter_results$labeled_source_mat_ids ~ "labeled",
      .default = "NA"
    )) |>
    # TODO line below: should it be mean or sum? They do lead to slightly different results
    dplyr::summarize(
      normalized_copies = mean(normalized_copies),
      timepoint = unique(timepoint), # weird to use unique(), other options?
      .by = c(feature_id, type)
    ) |>
    tidyr::pivot_wider(names_from = type, values_from = normalized_copies) |>
    dplyr::mutate(N_total_it = labeled + unlabeled)

  normalized_copies <- time_zero_totals |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features) |>
    dplyr::left_join(time_i_totals, by = "feature_id")


  normalized_copies <- normalized_copies |>
    dplyr::mutate(unlabeled = ifelse(is.na(unlabeled), 0, unlabeled)) |>
    dplyr::mutate(labeled = ifelse(is.na(labeled), 0, labeled)) |>
    dplyr::mutate(N_total_it = ifelse(is.na(N_total_it), 0, N_total_it))

  EAFs <- qsip_data_object@EAF |>
    dplyr::mutate(resample = ifelse(is.na(resample), 0, resample))


  # warn feature_id in normalized_copies not found in EAFs
  no_EAF_values <- normalized_copies |>
    dplyr::filter(!feature_id %in% EAFs$feature_id)

  if (nrow(no_EAF_values) > 0) {
    warning(glue::glue("{nrow(no_EAF_values)} feature_ids have zero abundance at this timepoint and have no EAF values. These values have been filtered out and added to @growth$no_EAF_values"), call. = FALSE)
    qsip_data_object@growth$no_EAF_values <- no_EAF_values
  }

  # can overwrite unlabeled using EAF values or not
  N_eaf <- normalized_copies |>
    dplyr::left_join(EAFs, by = "feature_id")

  rbd_3 <- N_eaf

  rbd_3 <- rbd_3 |>
    dplyr::mutate(M_heavy = (12.07747 * qsip_data_object@growth$propO) + M) |>
    dplyr::mutate(unlabeled = N_total_it * ((M_heavy - M_labeled) / (M_heavy - M))) |>
    dplyr::mutate(N_total_it = labeled + unlabeled) |>
    dplyr::mutate(time_diff = timepoint - timepoint1)

  negative_unlabeled <- rbd_3 |>
    dplyr::filter(unlabeled <= 0)

  negative_labeled <- rbd_3 |>
    dplyr::filter(labeled <= 0)

  if (nrow(negative_unlabeled) > 0) {
    warning(glue::glue("{nrow(negative_unlabeled)} calculated values of unlabeled samples are negative. These values have been filtered out and added to @growth$negative_unlabeled"), call. = FALSE)
    qsip_data_object@growth$negative_unlabeled <- negative_unlabeled
  }

  if (nrow(negative_labeled) > 0) {
    warning(glue::glue("{nrow(negative_labeled)} calculated values of labeled samples are negative. These values have been filtered out and added to @growth$negative_labeled"), call. = FALSE)
    qsip_data_object@growth$negative_labeled <- negative_labeled
  }

  # then, working with just the unlabeled that are greater than 0
  rbd_3 <- rbd_3 |>
    dplyr::filter(unlabeled > 0) |>
    dplyr::filter(labeled > 0) |>
    dplyr::mutate(
      di = log(unlabeled / N_total_i0) * (1 / (timepoint - timepoint1)),
      bi = log(N_total_it / unlabeled) * (1 / (timepoint - timepoint1)),
      ri = di + bi,
      r_net = N_total_it - N_total_i0
    ) |>
    dplyr::select(feature_id, timepoint1, timepoint2 = timepoint, resample, N_total_i0, N_total_it, unlabeled, r_net, bi, di, ri)

  # mark observed and resamples similar to other qSIP2 objects
  rbd_3 <- rbd_3 |>
    dplyr::mutate(observed = ifelse(resample == 0, TRUE, FALSE)) |>
    dplyr::mutate(resample = ifelse(resample == 0, NA, resample))


  qsip_data_object@growth$rates <- rbd_3

  return(qsip_data_object)
}






#' Summarize growth values
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