#' qSIP master data class
#'
#' A class to hold and validate qSIP data.
#'
#' This `qSIP2` object holds the source, sample and feature data, and does some
#' validation that the necessary source_mat_ids and sample_ids are correctly
#' shared.
#'
#' @slot source_data (*qsip_source_data*) A qSIP source data object
#' @slot sample_data (*qsip_sample_data*) A qSIP sample data object
#' @slot feature_data (*qsip_feature_data*) A qSIP feature data object
#'
#' @export
#'
#' @family "qSIP classes"
#'
#' @return A validated `qsip_data` object

qsip_data <- S7::new_class(
  "qsip_data",
  properties = list(
    source_data = S7::class_any,
    sample_data = S7::class_any,
    feature_data = S7::class_any,
    tube_rel_abundance = S7::class_data.frame,
    filtered_feature_data = S7::class_data.frame,
    filter_results = S7::class_list,
    wads = S7::class_data.frame,
    boots = S7::class_data.frame,
    results = S7::class_data.frame
  ),
  constructor = function(source_data,
                         sample_data,
                         feature_data) {

    # calculate tube level relative abundances
    tube_rel_abundance = feature_data@data |> # start with raw feature data
      pivot_longer(cols = c(everything(), -feature_id), # pivot longer
                   names_to = "sample_id",
                   values_to = "raw_abundance") |>
      filter(raw_abundance > 0) |> # remove features with no abundance
      group_by(sample_id) |> # group to calculate per-sample relative abundance
      mutate(rel_abundance = raw_abundance / sum(raw_abundance)) |> # do the calculation
      ungroup() |>
      left_join(sample_data@data, by = "sample_id") %>% # add sample data to get the source_mat_id
      filter(!is.na(source_mat_id)) |> # remove features that do not have a source id (this removes features found in the feature table but not the metadata)
      left_join(source_data@data, by = "source_mat_id") |> # combine
      select(feature_id, sample_id, rel_abundance, source_mat_id, gradient_pos_density, gradient_pos_rel_amt, isotope) %>%
      group_by(feature_id, source_mat_id, isotope) |>
      mutate(tube_rel_abundance = rel_abundance * gradient_pos_rel_amt) |> # takes the sample-adjusted abundances and gets the source-adjusted abundances
      ungroup() |>
      select(feature_id, sample_id, tube_rel_abundance) |>
      pivot_wider(names_from = sample_id, values_from = tube_rel_abundance, values_fill = 0)

    S7::new_object(S7::S7_object(),
                   source_data = source_data,
                   sample_data = sample_data,
                   feature_data = feature_data,
                   tube_rel_abundance = tube_rel_abundance,
                   filtered_feature_data = data.frame(),
                   filter_results = list(),
                   wads = data.frame(),
                   boots = data.frame(),
                   results = data.frame())
    },
  validator = function(self) {

    # make sure all are valid objects
    S7::validate(self@source_data)
    S7::validate(self@sample_data)
    S7::validate(self@feature_data)

    # make sure the objects are of the right type
    if (!"qsip_source_data" %in% class(self@source_data)) {
      stop("source_data must be of type qsip_source_data")
    } else if (!"qsip_sample_data" %in% class(self@sample_data)) {
      stop("sample_data must be of type qsip_sample_data")
    } else if (!"qsip_feature_data" %in% class(self@feature_data)) {
      stop("feature_data must be of type qsip_feature_data")
    }

    # # make sure source_mat_ids are shared
    # source_source_mat_id = self@source_data@data |> dplyr::pull(source_mat_id)
    # sample_source_mat_id = self@sample_data@data |> dplyr::pull(source_mat_id)
    #
    # if (length(setdiff(source_source_mat_id, sample_source_mat_id) > 0)) {
    #   stop("source_data has source_mat_id that are missing from the sample_data")
    # } else if (length(setdiff(sample_source_mat_id, source_source_mat_id) > 0)) {
    #   stop("sample_data has source_mat_id that are missing from the source_data")
    # }
    #
    # # make sure sample_ids are shared
    # sample_sample_id = self@sample_data@data |> dplyr::pull(sample_id)
    # feature_sample_id = self@feature_data@data |>
    #   dplyr::select(-feature_id) %>%
    #   colnames()
    #
    # if (length(setdiff(sample_sample_id, feature_sample_id) > 0)) {
    #   return(setdiff(sample_sample_id, feature_sample_id))
    # } else if (length(setdiff(feature_sample_id, sample_sample_id) > 0)) {
    #   return(setdiff(feature_sample_id, sample_sample_id))
    # }
  }
)

