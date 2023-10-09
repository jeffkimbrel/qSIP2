


run_EAF_calculations = function(qsip_data_object) {

  isotope = get_isotope_designation(qsip_data_object)

  # work with observed data
  observed_labeled = qsip_data_object@wads |>
    dplyr::filter(source_mat_id %in% qsip_data_object@filter_results$labeled_source_mat_ids) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features) |>
    dplyr::group_by(feature_id) |>
    dplyr::summarize(W_lab_mean = mean(WAD))

  observed_unlabeled = qsip_data_object@wads |>
    dplyr::filter(source_mat_id %in% qsip_data_object@filter_results$unlabeled_source_mat_ids) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features) |>
    dplyr::group_by(feature_id) |>
    dplyr::summarize(W_unlab_mean = mean(WAD))

  observed = observed_unlabeled |>
    dplyr::left_join(observed_labeled, by = "feature_id") |>
    dplyr::mutate(resample = NA, .after = feature_id) |>
    dplyr::mutate(observed = T)

  # work with resamples
  ## labeled
  p = dplyr::bind_rows(qsip_data_object@resamples$l, .id = "resample")
  p = dplyr::mutate(p,
                    W_lab_mean = rowMeans(dplyr::select(p, dplyr::starts_with('labeled_')),
                                          na.rm = TRUE)) |>
    dplyr::select(feature_id, resample, W_lab_mean)

  # unlabeled
  p2 = dplyr::bind_rows(qsip_data_object@resamples$u, .id = "resample")
  p2 = dplyr::mutate(p2,
                     W_unlab_mean = rowMeans(dplyr::select(p2, dplyr::starts_with('unlabeled_')),
                                             na.rm = TRUE)) |>
    dplyr::select(feature_id, resample, W_unlab_mean)

  EAF = p |>
    dplyr::left_join(p2, by = c("feature_id", "resample")) |>
    dplyr::mutate(observed = F) |>
    rbind(observed) |>
    dplyr::mutate(Z = calculate_Z(W_lab_mean, W_unlab_mean)) |> # hungate equation 4
    dplyr::mutate(G = calculate_gc_from_density(W_unlab_mean)) |> # hungate equation 5
    dplyr::mutate(M = calculate_M(G)) |> # hungate equation 6
    dplyr::mutate(atom_count = calculate_atoms(G, isotope)) |>
    dplyr::mutate(M_labeledmax = calculate_M_labeledmax(M, atom_count, isotope)) |>
    dplyr::mutate(M_labeled = calculate_M_labeled(M, W_lab_mean, W_unlab_mean)) |>
    dplyr::mutate(EAF = calculate_EAF(M_labeled = M_labeled, M = M, M_labeledmax = M_labeledmax, isotope = isotope))

  qsip_data_object@EAF = EAF
  qsip_data_object
}
