#' Calculate EAF values
#'
#' This is the last main workhorse function in qSIP analysis and generates the important
#' values (Z, G, M, M_labeledmax, M_labeled and EAF) for the observed data as well
#' as for all of the resampled data.
#'
#' @param qsip_data_object (*qsip_data*) A qsip_data_object with resample information
#' @param gc_method (*string*) The method to use for calculating the GC content from WAD
#' @param propO (*numeric*) The proportion of heavy isotope in the labeled DNA. Only used for 18O.
#'
#' @export
#'
#' @returns Returns an updated `qsip_data_object` with final EAF and other values
#' in the `@EAF` slot.

run_EAF_calculations <- function(qsip_data_object, gc_method = "MM", propO = 1) {

  # make sure the right data type and has been filtered and resampled
  if (!"qsip_data" %in% class(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = FALSE)
  } else if (length(qsip_data_object@resamples) == 0) {
    stop("qsip_data_object should be run through run_feature_filter() and run_resampling() functions first", call. = FALSE)
  }

  # bind variables
  source_mat_id <- feature_id <- WAD <- resample <- W_lab_mean <- W_unlab_mean <- Z <- G <- M <- atom_count <- M_labeledmax <- M_labeled <- NULL

  # get the source material ids corresponding to the labeled and unlabeled
  unlabeled_source_mat_ids = qsip_data_object@filter_results$unlabeled_source_mat_ids
  labeled_source_mat_ids = qsip_data_object@filter_results$labeled_source_mat_ids

  isotope <- get_isotope_designation(qsip_data_object, unlabeled_source_mat_ids, labeled_source_mat_ids)

  # work with observed data
  observed_labeled <- qsip_data_object@wads |>
    dplyr::filter(source_mat_id %in% labeled_source_mat_ids) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features) |>
    dplyr::group_by(feature_id) |>
    dplyr::summarize(W_lab_mean = mean(WAD))

  observed_unlabeled <- qsip_data_object@wads |>
    dplyr::filter(source_mat_id %in% unlabeled_source_mat_ids) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features) |>
    dplyr::group_by(feature_id) |>
    dplyr::summarize(W_unlab_mean = mean(WAD))

  observed <- observed_unlabeled |>
    dplyr::left_join(observed_labeled, by = "feature_id") |>
    dplyr::mutate(resample = NA, .after = feature_id) |>
    dplyr::mutate(observed = T)

  # work with resamples
  ## labeled
  p <- dplyr::bind_rows(qsip_data_object@resamples$l, .id = "resample")
  p <- dplyr::mutate(p,
    W_lab_mean = rowMeans(dplyr::select(p, dplyr::starts_with("labeled_")),
      na.rm = TRUE
    )
  ) |>
    dplyr::select(feature_id, resample, W_lab_mean)

  # unlabeled
  p2 <- dplyr::bind_rows(qsip_data_object@resamples$u, .id = "resample")
  p2 <- dplyr::mutate(p2,
    W_unlab_mean = rowMeans(dplyr::select(p2, dplyr::starts_with("unlabeled_")),
      na.rm = TRUE
    )
  ) |>
    dplyr::select(feature_id, resample, W_unlab_mean)

  p3 = p |>
    dplyr::left_join(p2, by = c("feature_id", "resample"))

  # remove bad values if allow resampling failures is true
  if (isTRUE(qsip_data_object@resamples$allow_failures)) {
    p3 = p3 |>
      dplyr::filter(!is.na(W_unlab_mean)) |>
      dplyr::filter(!is.na(W_lab_mean))
  }

  EAF <- p3 |>
    dplyr::mutate(observed = F) |>
    rbind(observed) |>
    dplyr::mutate(Z = calculate_Z(W_lab_mean, W_unlab_mean)) |> # hungate equation 4
    dplyr::mutate(G = calculate_gc_from_density(W_unlab_mean, method = gc_method)) |> # hungate equation 5
    dplyr::mutate(M = calculate_M(G)) |> # hungate equation 6
    dplyr::mutate(atom_count = calculate_atoms(G, isotope)) |>
    dplyr::mutate(M_labeledmax = calculate_M_labeledmax(M, atom_count, isotope, propO = propO)) |>
    dplyr::mutate(M_labeled = calculate_M_labeled(M, W_lab_mean, W_unlab_mean)) |>
    dplyr::mutate(EAF = calculate_EAF(M_labeled = M_labeled, M = M, M_labeledmax = M_labeledmax, isotope = isotope))

  qsip_data_object@EAF <- EAF
  qsip_data_object@growth$propO <- propO
  qsip_data_object
}
