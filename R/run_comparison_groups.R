#' Run comparison groups
#'
#' This function takes a dataframe of groups and parameters and runs an entire qSIP2 EAF workflow for each group.
#'
#' The heart of this function is the group dataframe, which should contain the following columns:
#' - group (required): a unique identifier for each group. This can be short, or a descriptive string describing the group
#' - unlabeled (required): a comma-separated list of `source_mat_id`s for the unlabeled isotope. Optionally, you can use terms such as "unlabeled" or "12C" to use all `source_mat_id`s with that isotope designation
#' - labeled (required): a comma-separated list of `source_mat_id`s for the labeled isotope
#'
#' Additionally, other optional columns can be included in the dataframe to set per group parameters:
#' - min_unlabeled_sources: the minimum number of unlabeled sources required for each fraction
#' - min_labeled_sources: the minimum number of labeled sources required for each fraction
#' - min_unlabeled_fractions: the minimum number of unlabeled fractions required for each source
#' - min_labeled_fractions: the minimum number of labeled fractions required for each source
#' - allow_failures: whether to allow failures in the filtering step
#' - resamples: the number of resamples to run
#' - seed: the seed for the resampling
#'
#' The last three in the list can also be added as optional parameters, and these will override any values in the groups dataframe.
#'
#' @param groups A dataframe containing group, unlabeled, labeled, and other optional columns
#' @param qsip_data_object A qsip_data object
#' @param allow_failures Whether to allow failures in the filtering step. Sets for all groups.
#' @param seed The seed for the resampling. Sets for all groups.
#' @param resamples The number of resamples to run. Sets for all groups.
#'
#' @export

run_comparison_groups <- function(groups,
                                  qsip_data_object,
                                  allow_failures = NULL,
                                  seed = NULL,
                                  resamples = NULL) {

  is_qsip_data(qsip_data_object)

  # groups dataframe should contain group, unlabeled and labeled columns, and there can be others
  required_cols <- c("group", "unlabeled", "labeled")
  if (!all(required_cols %in% colnames(groups))) {
    stop(glue::glue("Missing required column names in groups dataframe: {setdiff(required_cols, colnames(groups))}"))
  }

  # groups$group column should be unique
  if (length(unique(groups$group)) != nrow(groups)) {
    stop("group column must be unique", call. = FALSE)
  }

  # bind variables
  unlabeled <- labeled <- value <- . <- NULL

  # make sure all source_mat_ids in the isotope columns are in the qsip_data_object
  ## first, fill in "wildcard" source_mat_ids
  groups = groups |>
    dplyr::mutate(unlabeled = dplyr::case_match(
      unlabeled,
      "unlabeled" ~ paste(get_all_by_isotope(qsip_data_object, "unlabeled", silent = TRUE), collapse = ","),
      "12C" ~ paste(get_all_by_isotope(qsip_data_object, "12C", silent = TRUE), collapse = ","),
      "14N" ~ paste(get_all_by_isotope(qsip_data_object, "14N", silent = TRUE), collapse = ","),
      "16O" ~ paste(get_all_by_isotope(qsip_data_object, "16O", silent = TRUE), collapse = ","),
      .default = unlabeled
    ))  |>
    dplyr::mutate(labeled = dplyr::case_match(
      labeled,
      "labeled" ~ paste(get_all_by_isotope(qsip_data_object, "labeled", silent = TRUE), collapse = ","),
      "13C" ~ paste(get_all_by_isotope(qsip_data_object, "13C", silent = TRUE), collapse = ","),
      "15N" ~ paste(get_all_by_isotope(qsip_data_object, "15N", silent = TRUE), collapse = ","),
      "18O" ~ paste(get_all_by_isotope(qsip_data_object, "18O", silent = TRUE), collapse = ","),
      .default = labeled
    ))

  # add optional parameters to overwrite the groups dataframe, or set values if not in the groups dataframe
  if (!is.null(allow_failures)) {
    groups$allow_failures <- allow_failures
  }
  if (!is.null(seed)) {
    groups$seed <- seed
  }
  if (!is.null(resamples)) {
    groups$resamples <- resamples
  }


  source_mat_ids_in_groups <- unlist(groups[, colnames(groups) %in% c("labeled", "unlabeled")]) |>
    tibble::enframe() |>
    tidyr::separate_rows(value, sep = ",") |>
    dplyr::mutate(value = stringr::str_trim(value)) |>
    dplyr::pull(value)

  if (length(setdiff(source_mat_ids_in_groups, get_source_mat_ids(qsip_data_object))) > 0) {
    stop("Invalid source_mat_ids in group dataframe")
  }

  group_list <- groups |>
    split(.$group)

  # print the name of each element in group_list
  multi_qsip_list <- purrr::map2(group_list, names(group_list), \(i, j) multi_qsip_wrapper_launcher(i, j, qsip_data_object),
    .progress = list(
      type = "tasks",
      format = "Finished groups {cli::pb_bar} {cli::pb_percent}",
      clear = F
    )
  )

  if (isTRUE(is_qsip_data_list(multi_qsip_list))) {
    return(multi_qsip_list)
  } else {
    stop("Something went wrong with run_comparison_groups()")
  }
}


