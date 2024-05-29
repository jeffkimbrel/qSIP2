#' Wrapper for running the qSIP EAF workflow
#'
#' @param qsip_data_object A qsip_data object
#' @param group A group name
#' @param unlabeled_source_mat_ids A vector of source_mat_ids for the unlabeled isotope
#' @param labeled_source_mat_ids A vector of source_mat_ids for the labeled isotope
#' @param min_unlabeled_sources The minimum number of sources for the unlabeled isotope
#' @param min_labeled_sources The minimum number of sources for the labeled isotope
#' @param min_unlabeled_fractions The minimum number of fractions for the unlabeled isotope
#' @param min_labeled_fractions The minimum number of fractions for the labeled isotope
#' @param allow_failures Whether to allow failures in the filtering step
#' @param resamples The number of resamples to run
#' @param seed The seed for the resampling
#'
#' @export

multi_qsip_wrapper = function(qsip_data_object,
                            group = NULL,
                            unlabeled_source_mat_ids,
                            labeled_source_mat_ids,
                            min_unlabeled_sources = 2,
                            min_labeled_sources = 2,
                            min_unlabeled_fractions = 2,
                            min_labeled_fractions = 2,
                            allow_failures = FALSE,
                            resamples = 1000,
                            seed = NULL
) {



  run_feature_filter(qsip_data_object = qsip_data_object,
                     group = group,
                     unlabeled_source_mat_ids = unlabeled_source_mat_ids,
                     labeled_source_mat_ids = labeled_source_mat_ids,
                     min_unlabeled_sources = min_unlabeled_sources,
                     min_labeled_sources = min_labeled_sources,
                     min_unlabeled_fractions = min_unlabeled_fractions,
                     min_labeled_fractions = min_labeled_fractions,
                     quiet = TRUE
  ) |>
    run_resampling(allow_failures = allow_failures,
                   resamples = resamples,
                   with_seed = seed,
                   progress = FALSE,
                   quiet = TRUE) |>
    run_EAF_calculations()

}


#' Internal function for launching multi_qsip_wrapper()
#'
#' @param group A group dataframe
#' @param name A group name
#' @param qsip_data_object A qsip_data object
#' @param min_unlabeled_sources The minimum number of sources for the unlabeled isotope
#' @param min_labeled_sources The minimum number of sources for the labeled isotope
#' @param min_unlabeled_fractions The minimum number of fractions for the unlabeled isotope
#' @param min_labeled_fractions The minimum number of fractions for the labeled isotope
#' @param allow_failures Whether to allow failures in the filtering step
#' @param resamples The number of resamples to run
#' @param seed The seed for the resampling
#'
#' @export

multi_qsip_wrapper_launcher <- function(group,
                               name,
                               qsip_data_object,
                               min_unlabeled_sources = 2,
                               min_labeled_sources = 2,
                               min_unlabeled_fractions = 2,
                               min_labeled_fractions = 2,
                               allow_failures = FALSE,
                               resamples = 1000,
                               seed = NULL) {

  # get the unlabeled and labeled source_mat_ids, split by comma into vectors, and remove whitespace
  unlabeled = group$unlabeled |> stringr::str_split(",") |> unlist() |> stringr::str_trim()
  labeled = group$labeled |> stringr::str_split(",") |> unlist() |> stringr::str_trim()

  # if seed is in the group dataframe, use it
  if ("seed" %in% colnames(group)) {
    seed = dplyr::pull(group, seed)
  }

  # if min_XXX_sources is in the group dataframe, use it
  if ("min_unlabeled_sources" %in% colnames(group)) {
    min_unlabeled_sources = dplyr::pull(group, min_unlabeled_sources)
  }
  if ("min_labeled_sources" %in% colnames(group)) {
    min_labeled_sources = dplyr::pull(group, min_labeled_sources)
  }

  # if min_XXX_fractions is in the group dataframe, use it
  if ("min_unlabeled_fractions" %in% colnames(group)) {
    min_unlabeled_fractions = dplyr::pull(group, min_unlabeled_fractions)
  }
  if ("min_labeled_fractions" %in% colnames(group)) {
    min_labeled_fractions = dplyr::pull(group, min_labeled_fractions)
  }

  if ("allow_failures" %in% colnames(group)) {
    allow_failures = as.logical(dplyr::pull(group, allow_failures))
  }

  if ("resamples" %in% colnames(group)) {
    resamples = as.integer(dplyr::pull(group, resamples))
  }

  multi_qsip_wrapper(qsip_data_object,
                   group = name,
                   unlabeled_source_mat_ids = unlabeled,
                   labeled_source_mat_ids = labeled,
                   min_unlabeled_sources = min_unlabeled_sources,
                   min_labeled_sources = min_labeled_sources,
                   min_unlabeled_fractions = min_unlabeled_fractions,
                   min_labeled_fractions = min_labeled_fractions,
                   allow_failures = allow_failures,
                   resamples = resamples,
                   seed = seed
  )
}
