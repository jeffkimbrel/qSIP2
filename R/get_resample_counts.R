#' Get counts of successful resampling
#'
#' For each feature_id and label type, this function will return the counts of successful
#' resampling. This value will typically be the number of resamples given to `run_resampling()`,
#' but if `run_resampling()` is called with `allow_failures = TRUE` then the number of
#' successful resamples might be less than the number of resamples given.
#'
#' If as_percentage is TRUE, the counts will be returned as a percentage of
#' the total number of resamples.
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been resampled
#' @param as_percentage (*logical*) If TRUE, the counts will be returned as a percentage of the total number of resamples
#'
#'
#' @export
#'
#' @returns A dataframe with columns feature_id, type, and n

get_resample_counts = function(qsip_data_object,
                               as_percentage = FALSE) {

  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  } else if (length(qsip_data_object@resamples) == 0) {
    stop("this function requires a qsip object that has been run through run_resampling()", call. = FALSE)
  }

  # bind variables
  feature_id <- type <- n_counts <- labeled <- unlabeled <- NULL

  u = dplyr::bind_rows(qsip_data_object@resamples$u, .id = "resample")
  u = u[rowSums(is.na(u)) != ncol(u) - 3, ] |>
    dplyr::select(feature_id, type)

  l = dplyr::bind_rows(qsip_data_object@resamples$l, .id = "resample")
  l = l[rowSums(is.na(l)) != ncol(l) - 3, ] |>
    dplyr::select(feature_id, type)

  counts = rbind(u, l) |>
    dplyr::group_by(feature_id, type) |>
    dplyr::count(name = "n_counts") |>
    dplyr::ungroup()

  if (isTRUE(as_percentage)) {
    counts = counts |>
      dplyr::mutate(n_counts = n_counts / qsip_data_object@resamples$n)
  }

  counts = counts |>
    tidyr::pivot_wider(names_from = "type", values_from = "n_counts") |>
    dplyr::rename("labeled_resamples" = labeled,
                  "unlabeled_resamples" = unlabeled)


  return(counts)
}
