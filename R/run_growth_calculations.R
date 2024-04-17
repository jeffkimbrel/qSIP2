#' Calculate time zero feature totals
#'
#' @param qsip_data_object (*qsip_data*) An object of `qsip_data` class
#' @param time (*character*) The name of the time column in the source data
#' @param value (*numeric*) The value of the time column to filter on
#'
#' @export
#'


calculate_time_zero_totals <- function(qsip_data_object,
                                       time = "time",
                                       value = 0) {

  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  }




  N_total_i0 = qsip_data_object@tube_rel_abundance |>
    dplyr::summarize(REL = sum(tube_rel_abundance), .by = c(feature_id, source_mat_id)) |>
    dplyr::left_join(qsip_data_object@source_data@data, by = "source_mat_id") |>
    dplyr::select(
      time_column = all_of(time),
      dplyr::everything()
    ) |>
    dplyr::filter(time_column == value) |>
    dplyr::mutate(N_total_i0 = REL * total_abundance) |>
    dplyr::select(feature_id, source_mat_id, time_column, N_total_i0) |>
    dplyr::summarize(N_total_i0 = sum(N_total_i0), .by = feature_id)

  return(N_total_i0)
}

#' Run growth calculations
#'
#' @param qsip_data_object (*qsip_data*) An object of `qsip_data` class
#' @param time_zero_totals (*data.frame*) A data frame of time zero totals from `calculate_time_zero_totals()`
#'
#' @export
#'

run_growth_calculations = function(qsip_data_object,
                                   time_zero_totals,
                                   time = "time") {


  normalized_copies = qsip_data_object@tube_rel_abundance |>
    dplyr::summarize(REL = sum(tube_rel_abundance), .by = c(feature_id, source_mat_id)) |>
    dplyr::left_join(qsip_data_object@source_data@data, by = "source_mat_id") |>
    dplyr::select(
      time_column = all_of(time),
      dplyr::everything()
    ) |>
    dplyr::mutate(normalized_copies = REL * total_abundance) |>
    dplyr::select(feature_id, source_mat_id, time_column, normalized_copies) |>
    dplyr::mutate(type = dplyr::case_when(
      source_mat_id %in% qsip_data_object@filter_results$unlabeled_source_mat_ids ~ "unlabeled",
      source_mat_id %in% qsip_data_object@filter_results$labeled_source_mat_ids ~ "labeled",
      .default = "NA"
    )) |>
    dplyr::filter(type != "NA")

  N = normalized_copies |>
    dplyr::summarize(normalized_copies = sum(normalized_copies), .by = c(feature_id, type)) |>
    tidyr::pivot_wider(names_from = type, values_from = normalized_copies) |>
    dplyr::mutate(N_total_it = labeled + unlabeled) |>
    dplyr::left_join(time_zero_totals, by = "feature_id")


  EAFs = qsip_data_object@EAF |>
    dplyr::filter(observed == TRUE) |>
    dplyr::select(-resample, -observed) |>
    tidyr::pivot_longer(cols = c(everything(), -feature_id)) |>
    dplyr::summarize(value = mean(value), .by = c(feature_id, name)) |>
    tidyr::pivot_wider()

  N_eaf = N |>
    dplyr::left_join(EAFs, by = "feature_id")


  rbd_3 = N_eaf |>
    dplyr::mutate(M_heavy = (12.07747 * qsip_data_object@growth$propO) + M) |>
    dplyr::mutate(unlabeled =  N_total_it * ((M_heavy - M_labeled) / (M_heavy - M))) |>
    dplyr::mutate(di = log(unlabeled / N_total_i0) * (1 / 10),
           bi = log(N_total_it / unlabeled) * (1 / 10),
           ri = di + bi) |>
    dplyr::select(feature_id, bi, di, ri)

  qsip_data_object@growth$rates = rbd_3

  return(qsip_data_object)

}




