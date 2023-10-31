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


#' Find shared sources and samplesin qSIP objects
#'
#' @export


find_shared_ids <- function(source_data, sample_data, feature_data) {
  missing <- FALSE

  # Report the source ids shared and unique
  source_source_mat_id <- source_data@data |>
    dplyr::pull(source_mat_id) |>
    unique()
  sample_source_mat_id <- sample_data@data |>
    dplyr::pull(source_mat_id) |>
    unique()

  shared <- list(
    "sources" = list("shared" = c(), "source_data" = c(), "sample_data" = c()),
    "samples" = list("shared" = c(), "sample_data" = c(), "feature_id" = c())
  )

  shared$sources$shared <- intersect(source_source_mat_id, sample_source_mat_id)

  if (isTRUE(setequal(source_source_mat_id, sample_source_mat_id))) {
    message(glue::glue_col("{green There are {length(source_source_mat_id)} source_mat_ids, and they are all shared between the source and sample objects}"))
  } else {
    if (length(setdiff(source_source_mat_id, sample_source_mat_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: source_data has {length(setdiff(source_source_mat_id, sample_source_mat_id))} source_mat_id(s) that are missing from the sample_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(source_source_mat_id, sample_source_mat_id), collapse = ', ')}}"))
      shared$sources$source_data <- setdiff(source_source_mat_id, sample_source_mat_id)
      missing <- TRUE
    }

    if (length(setdiff(sample_source_mat_id, source_source_mat_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: sample_data has {length(setdiff(sample_source_mat_id, source_source_mat_id))} source_mat_id(s) that are missing from the source_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(sample_source_mat_id, source_source_mat_id), collapse = ', ')}}"))
      shared$sources$sample_data <- setdiff(sample_source_mat_id, source_source_mat_id)
      missing <- TRUE
    }
  }

  # Report the sample ids shared and unique
  sample_sample_id <- sample_data@data |>
    dplyr::pull(sample_id) |>
    unique()
  feature_sample_id <- feature_data@data |>
    dplyr::select(-feature_id) %>%
    colnames()

  shared$samples$shared <- intersect(sample_sample_id, feature_sample_id)
  if (isTRUE(setequal(sample_sample_id, feature_sample_id))) {
    message(glue::glue_col("{green There are {length(sample_sample_id)} feature_ids, and they are all shared between the sample and feature objects}"))
  } else {
    if (length(setdiff(sample_sample_id, feature_sample_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: sample_data has {length(setdiff(sample_sample_id, feature_sample_id))} feature_id(s) that are missing from the feature_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(sample_sample_id, feature_sample_id), collapse = ', ')}}"))
      shared$samples$sample_data <- setdiff(sample_sample_id, feature_sample_id)
      missing <- TRUE
    }

    if (length(setdiff(feature_sample_id, sample_sample_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: feature_data has {length(setdiff(feature_sample_id, sample_sample_id))} feature_id(s) that are missing from the sample_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(feature_sample_id, sample_sample_id), collapse = ', ')}}"))
      shared$samples$feature_id <- setdiff(feature_sample_id, sample_sample_id)
      missing <- TRUE
    }
  }

  if (isTRUE(missing)) {
    message()
    message(glue::glue_col("{yellow ***Missing sources/samples have not been removed from the dataset***}"))
    message(glue::glue_col("{yellow ***Run show_unshared_ids(<qsip_data_object>) to show IDs missing from datasets***}"))  }

  return(shared)
}


#' Show source and sample ID overlaps
#'
#' @export

show_unshared_ids <- function(qsip_data_object) {

  shared = qsip_data_object@shared

  message(glue::glue('{length(shared$sources$source_data)} source_mat_id(s) unique to source_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$sources$source_data, collapse = ", "))

  message(glue::glue('\n\n{length(shared$sources$sample_data)} source_mat_id(s) unique to sample_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$sources$sample_data, collapse = ", "))

  message(glue::glue('\n\n{length(shared$samples$sample_data)} sample_id(s) unique to sample_data:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$samples$sample_data, collapse = ", "))

  message(glue::glue('\n\n{length(shared$samples$feature_id)} sample_id(s) unique to feature_id:\n=+=+=+=+=+=+=+=+=+=+='))
  message(paste(shared$samples$feature_id, collapse = ", "))
}
