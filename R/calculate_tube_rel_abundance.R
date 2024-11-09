#' Calculate tube relative abundance (internal)
#'
#' The "tube level relative abundance" has two layers of relative abundance. It first
#' takes the `raw_abundance` counts per sample, and divides by the total to get
#' the `rel_abundance` values. Next, it takes these values and divides them by the
#' `gradient_pos_rel_amt` values stored at the source level which normalizes the `rel_abundance`
#' values by the amount of sample per fraction. This is the final value stored in
#' the `tube_rel_abundance` column.
#'
#' To speed up the calculations, this function removes `feature_ids` in a sample
#' if the abundance is zero.
#'
#' v0.10.3 updated this function to have different behavior for feature type 'normalized'.
#' This type should use the raw data in the feature table, putting those values into the
#' `tube_rel_abundance` column.
#'
#' @param source_data (*qsip_source_data*) A qSIP source data object
#' @param sample_data (*qsip_sample_data*) A qSIP sample data object
#' @param feature_data (*qsip_feature_data*) A qSIP feature data object
#'
#' @export
#'
#' @returns A long format dataframe with one row per `feature_id` per `sample_id`

calculate_tube_rel_abundance <- function(source_data, sample_data, feature_data) {

  # make sure the objects are of the right type
  stopifnot("source_data should be of class <qsip_source_data>" = inherits(source_data, c("qsip_source_data", "qSIP2::qsip_source_data")))
  stopifnot("sample_data should be of class <qsip_sample_data>" = inherits(sample_data, c("qsip_sample_data", "qSIP2::qsip_sample_data")))
  stopifnot("feature_data should be of class <qsip_feature_data>" = inherits(feature_data, c("qsip_feature_data", "qSIP2::qsip_feature_data")))

  # bind variables
  feature_id <- raw_abundance <- sample_id <- source_mat_id <- rel_abundance <- gradient_pos_density <- gradient_pos_rel_amt <- tube_rel_abundance <- isotope <- tube_rel_abundance <- NULL

  # extract dataframes
  feature_df <- feature_data@data
  sample_df <- sample_data@data |>
    dplyr::select(-dplyr::any_of("isotope")) # remove isotope column if it exists (addresses issue #5)
  source_df <- source_data@data

  # make sure feature data type has compatible value
  stopifnot("feature <type> is unknown" = feature_data@type %in% c("counts", "coverage", "normalized", "relative"))

  if (feature_data@type %in% c("counts", "coverage", "relative")) {
    feature_df |> # start with raw feature data
      tidyr::pivot_longer(
        cols = c(dplyr::everything(), -feature_id), # pivot longer
        names_to = "sample_id",
        values_to = "raw_abundance"
      ) |>
      dplyr::filter(raw_abundance > 0) |> # remove features with no abundance
      dplyr::group_by(sample_id) |> # group to calculate per-sample relative abundance
      dplyr::mutate(rel_abundance = raw_abundance / sum(raw_abundance)) |> # do the calculation
      dplyr::ungroup() |>
      dplyr::left_join(sample_df, by = "sample_id") |> # add sample data to get the source_mat_id
      dplyr::filter(!is.na(source_mat_id)) |> # remove features that do not have a source id (this removes features found in the feature table but not the metadata)
      dplyr::left_join(source_df, by = "source_mat_id") |> # combine
      dplyr::select(feature_id, sample_id, rel_abundance, source_mat_id, gradient_pos_density, gradient_pos_rel_amt, isotope) |>
      dplyr::group_by(feature_id, source_mat_id, isotope) |>
      dplyr::mutate(tube_rel_abundance = rel_abundance * gradient_pos_rel_amt) |> # takes the sample-adjusted abundances and gets the source-adjusted abundances
      dplyr::ungroup() |>
      dplyr::select(feature_id, sample_id, source_mat_id, tube_rel_abundance, gradient_pos_density, gradient_pos_rel_amt)
  } else if (feature_data@type == "normalized") {
    message("normalized")
    feature_df |> # start with raw feature data
      tidyr::pivot_longer(
        cols = c(dplyr::everything(), -feature_id), # pivot longer
        names_to = "sample_id",
        values_to = "raw_abundance"
      ) |>
      dplyr::filter(raw_abundance > 0) |> # remove features with no abundance
      dplyr::left_join(sample_df, by = "sample_id") |> # add sample data to get the source_mat_id
      dplyr::filter(!is.na(source_mat_id)) |> # remove features that do not have a source id (this removes features found in the feature table but not the metadata)
      dplyr::left_join(source_df, by = "source_mat_id") |>
      dplyr::group_by(sample_id) |>
      dplyr::mutate(gradient_pos_rel_amt = sum(raw_abundance)) |>
      dplyr::ungroup() |>
      dplyr::select(feature_id, sample_id, source_mat_id, tube_rel_abundance = raw_abundance, gradient_pos_density, gradient_pos_rel_amt)
  }
}
