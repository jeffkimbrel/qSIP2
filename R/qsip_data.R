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
#' @family "qSIP Objects"
#'
#' @return A validated `qsip_data` object

qsip_data <- S7::new_class(
  "qsip_data",
  properties = list(
    source_data = S7::class_any,
    sample_data = S7::class_any,
    feature_data = S7::class_any,
    shared = S7::class_list,
    tube_rel_abundance = S7::class_data.frame,
    wads = S7::class_data.frame,
    source_wads = S7::class_data.frame,
    fraction_counts = S7::class_data.frame,
    filtered_feature_data = S7::class_data.frame,
    filtered_wad_data = S7::class_data.frame,
    filter_results = S7::class_list,
    resamples = S7::class_list,
    EAF = S7::class_data.frame
  ),
  constructor = function(source_data,
                         sample_data,
                         feature_data) {

    # make sure data is correct
    stopifnot("ERROR: source_data should be of class qsip_source_data" = "qsip_source_data" %in% class(source_data))
    stopifnot("ERROR: sample_data should be of class qsip_sample_data" = "qsip_sample_data" %in% class(sample_data))
    stopifnot("ERROR: feature_data should be of class qsip_feature_data" = "qsip_feature_data" %in% class(feature_data))

    # calculate tube level relative abundances
    tube_rel_abundance <- calculate_tube_rel_abundance(source_data, sample_data, feature_data)
    wad_data <- calculate_wads(tube_rel_abundance)
    source_wad <- calculate_source_wads(sample_data)
    shared <- find_shared_ids(source_data, sample_data, feature_data)

    # XXX

    S7::new_object(S7::S7_object(),
      source_data = source_data,
      sample_data = sample_data,
      feature_data = feature_data,
      shared = shared,
      tube_rel_abundance = tube_rel_abundance,
      wads = wad_data$wads,
      source_wads = source_wad,
      fraction_counts = wad_data$fraction_counts,
      filtered_feature_data = data.frame(),
      filtered_wad_data = data.frame(),
      filter_results = list(),
      resamples = list(),
      EAF = data.frame()
    )
  },
  validator = function(self) {
    # make sure all are valid objects
    S7::validate(self@source_data)
    S7::validate(self@sample_data)
    S7::validate(self@feature_data)

    # make sure the objects are of the right type
    if (!"qsip_source_data" %in% class(self@source_data)) {
      stop("ERROR: source_data must be of type qsip_source_data")
    } else if (!"qsip_sample_data" %in% class(self@sample_data)) {
      stop("ERROR: sample_data must be of type qsip_sample_data")
    } else if (!"qsip_feature_data" %in% class(self@feature_data)) {
      stop("ERROR: feature_data must be of type qsip_feature_data")
    }
  }
)







