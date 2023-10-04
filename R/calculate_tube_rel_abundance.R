
#'
#'
#' To speed up the calculations, this function removes `feature_ids` in a sample
#' if the abundance is zero.


calculate_tube_rel_abundance = function(source_data, sample_data, feature_data) {

  # make sure all are valid objects
  S7::validate(source_data)
  S7::validate(sample_data)
  S7::validate(feature_data)

  # make sure the objects are of the right type
  if (!"qsip_source_data" %in% class(source_data)) {
    stop("source_data must be of type qsip_source_data")
  } else if (!"qsip_sample_data" %in% class(sample_data)) {
    stop("sample_data must be of type qsip_sample_data")
  } else if (!"qsip_feature_data" %in% class(feature_data)) {
    stop("feature_data must be of type qsip_feature_data")
  }

  feature_data@data |> # start with raw feature data
    tidyr::pivot_longer(cols = c(everything(), -feature_id), # pivot longer
                        names_to = "sample_id",
                        values_to = "raw_abundance") |>
    dplyr::filter(raw_abundance > 0) |> # remove features with no abundance
    dplyr::group_by(sample_id) |> # group to calculate per-sample relative abundance
    dplyr::mutate(rel_abundance = raw_abundance / sum(raw_abundance)) |> # do the calculation
    dplyr::ungroup() |>
    dplyr::left_join(sample_data@data, by = "sample_id") %>% # add sample data to get the source_mat_id
    dplyr::filter(!is.na(source_mat_id)) |> # remove features that do not have a source id (this removes features found in the feature table but not the metadata)
    dplyr::left_join(source_data@data, by = "source_mat_id") |> # combine
    dplyr::select(feature_id, sample_id, rel_abundance, source_mat_id, gradient_pos_density, gradient_pos_rel_amt, isotope) %>%
    dplyr::group_by(feature_id, source_mat_id, isotope) |>
    dplyr::mutate(tube_rel_abundance = rel_abundance * gradient_pos_rel_amt) |> # takes the sample-adjusted abundances and gets the source-adjusted abundances
    dplyr::ungroup() |>
    dplyr::select(feature_id, sample_id, source_mat_id, tube_rel_abundance, gradient_pos_density, gradient_pos_rel_amt)

}
