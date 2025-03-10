#' Filter features in a qSIP data object
#'
#' Filters a feature in a feature table in a `qsip_data` object by presence in
#' a minimum number of fractions and source_mat_ids.
#'
#' Filtering is first done on the fractions, and then on the source_mat_ids. For
#' example, take a feature that is found in three source_mat_ids in 3, 5 and 9
#' fractions. If you set `min_fractions = 5` and `min_sources = 3` then this
#' feature will not survive the filtering because although it is found in three
#' source_mat_ids, one of them is less then the minimum fraction count and would
#' therefore be considered not found in that source_mat_id.
#'
#' The feature table is filtered to retain only the feature_ids passing the
#' filter, and to keep only the sample_ids that correspond to the given
#' source_mat_ids. This filtered table is stored in the `@filtered_feature_data`
#' slot, and the values in this table are not the raw initial values but are the
#' relative abundances per fraction per tube. These values are originally
#' calculated during `qsip_data` object creation and the values for all features
#' are stored in the `@tube_rel_abundance` slot.
#'
#' @param qsip_data_object (*qsip_data*) An object of `qsip_data` class
#' @param group (*string*) An optional name to assign to this filtered group
#' @param unlabeled_source_mat_ids (*string or strings(s)*) A list of the unlabeled source_mat_ids to filter on
#' @param labeled_source_mat_ids (*string or strings(s)*) A list of the labeled source_mat_ids to filter on
#' @param min_unlabeled_sources (*integer, default: 2*) Minimum number of unlabeled source_mat_ids a feature must be found in.
#' @param min_labeled_sources (*integer, default: 2*) Minimum number of labeled source_mat_ids a feature must be found in.
#' @param min_unlabeled_fractions (*integer, default: 2*) Minimum number of fractions a feature must be found in to be present in an unlabeled source_mat_id
#' @param min_labeled_fractions (*integer, default: 2*) Minimum number of fractions a feature must be found in to be present in an labeled source_mat_id
#' @param quiet (*logical, default: FALSE*) Suppress messages
#'
#' @export
#'
#' @family "qSIP Filtering"
#'
#' @returns An updated `qsip_data` object with a filtered feature dataframe in the
#' `@filtered_feature_data` slot and intermediate data in the `@filter_results`
#' slot for plotting.

run_feature_filter <- function(qsip_data_object,
                               group = NULL,
                               unlabeled_source_mat_ids,
                               labeled_source_mat_ids,
                               min_unlabeled_sources = 2,
                               min_labeled_sources = 2,
                               min_unlabeled_fractions = 2,
                               min_labeled_fractions = 2,
                               quiet = FALSE) {

  # error if is_qsip_data(qsip_data_object) is not true
  if (isFALSE(is_qsip_data(qsip_data_object))) {
    stop("<qsip_data_object> must be of class qsip_data", call. = FALSE)
  }

  # make sure minimums are not bigger than possible
  if (min_labeled_sources > length(labeled_source_mat_ids)) {
    stop(glue::glue("min_labeled_sources is set to {min_labeled_sources} but labeled_source_mat_ids only has {length(labeled_source_mat_ids)}"))
  }

  if (min_unlabeled_sources > length(unlabeled_source_mat_ids)) {
    stop(glue::glue("min_unlabeled_sources is set to {min_unlabeled_sources} but unlabeled_source_mat_ids only has {length(unlabeled_source_mat_ids)}"))
  }

  # make sure all given source_mat_ids are found in sample_data
  if (length(setdiff(unlabeled_source_mat_ids, qsip_data_object@sample_data@data$source_mat_id) > 0)) {
    stop("Some given unlabeled_source_mat_ids are not found", call. = FALSE)
  } else if (length(setdiff(labeled_source_mat_ids, qsip_data_object@sample_data@data$source_mat_id) > 0)) {
    stop("Some given labeled_source_mat_ids are not found", call. = FALSE)
  }

  # make sure source_mat_ids match expected isotope types
  if (isFALSE(validate_source_isotope(
    qsip_data_object,
    unlabeled_source_mat_ids,
    c("12C", "14N", "16O")
  ))) {
    stop("some of the unlabeled_source_mat_ids have a heavy isotope designation", call. = FALSE)
  }
  if (isFALSE(validate_source_isotope(
    qsip_data_object,
    labeled_source_mat_ids,
    c("13C", "15N", "18O")
  ))) {
    stop("some of the labeled_source_mat_ids have a light isotope designation", call. = FALSE)
  }

  # bind variables
  feature_id <- source_mat_id <- tube_rel_abundance <- fraction_call <- type <- source_call <- labeled <- unlabeled <- sample_id <- WAD <- NULL

  source_mat_ids <- c(unlabeled_source_mat_ids, labeled_source_mat_ids)

  # extract tables
  initial_feature_id_count <- qsip_data_object@tube_rel_abundance |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()

  if (isFALSE(quiet)) {
    message(glue::glue("There are initially {initial_feature_id_count} unique feature_ids"))
  }

  # get long table and subset to only included source_mat_ids
  data <- qsip_data_object@tube_rel_abundance |>
    dplyr::filter(source_mat_id %in% source_mat_ids)

  secondary_feature_id_count <- data |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()

  if (isFALSE(quiet)) {
    message(glue::glue("{secondary_feature_id_count} of these have abundance in at least one fraction of one source_mat_id"))
  }

  if (isFALSE(quiet)) {
    message(rep("=+", 25))
    message("Filtering feature_ids by fraction...")
  }




  by_fraction <- data |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::summarize(
      n_fractions = dplyr::n(),
      tube_rel_abundance = sum(tube_rel_abundance),
      .groups = "drop"
    ) |>
    tidyr::complete(feature_id, # fill in missing fractions with 0
                    source_mat_id,
                    fill = list(
                      n_fractions = 0,
                      tube_rel_abundance = 0
                    )
    ) |>
    dplyr::mutate(type = dplyr::case_when(
      source_mat_id %in% unlabeled_source_mat_ids ~ "unlabeled",
      source_mat_id %in% labeled_source_mat_ids ~ "labeled"
    )) |>
    dplyr::mutate(fraction_call = dplyr::case_when(
      n_fractions == 0 ~ "Zero Fractions",
      type == "unlabeled" & n_fractions < min_unlabeled_fractions ~ "Fraction Filtered",
      type == "labeled" & n_fractions < min_labeled_fractions ~ "Fraction Filtered",
      type == "unlabeled" & n_fractions >= min_unlabeled_fractions ~ "Fraction Passed",
      type == "labeled" & n_fractions >= min_labeled_fractions ~ "Fraction Passed"
    ))

  if (isFALSE(quiet)) {
    fraction_results_message(by_fraction)
  }

  if (isFALSE(quiet)) {
    message(rep("=+", 25))
    message("Filtering feature_ids by source...")
  }

  by_source <- by_fraction |>
    dplyr::filter(fraction_call == "Fraction Passed") |>
    dplyr::group_by(feature_id, type) |>
    dplyr::filter(tube_rel_abundance > 0) |>
    dplyr::summarize(
      n_sources = dplyr::n(),
      mean_tube_rel_abundance = mean(tube_rel_abundance),
      .groups = "drop"
    ) |>
    tidyr::complete(feature_id,
                    type,
                    fill = list(
                      n_sources = 0,
                      tube_rel_abundance = 0
                    )
    ) |>
    dplyr::mutate(source_call = dplyr::case_when(
      n_sources == 0 ~ "Zero Sources",
      type == "unlabeled" & n_sources < min_unlabeled_sources ~ "Source Filtered",
      type == "labeled" & n_sources < min_labeled_sources ~ "Source Filtered",
      type == "unlabeled" & n_sources >= min_unlabeled_sources ~ "Source Passed",
      type == "labeled" & n_sources >= min_labeled_sources ~ "Source Passed"
    ))

  if (isFALSE(quiet)) {
    source_results_message(by_source)
  }

  retained_features <- by_source |>
    dplyr::select(feature_id, type, source_call) |>
    tidyr::pivot_wider(names_from = type, values_from = source_call) |>
    dplyr::filter(labeled == "Source Passed" & unlabeled == "Source Passed") |>
    dplyr::pull(feature_id)

  qsip_data_object@filter_results <- list(
    "group" = group,
    "source_filtered" = by_source,
    "fraction_filtered" = by_fraction,
    "retained_features" = retained_features,
    "labeled_source_mat_ids" = labeled_source_mat_ids,
    "unlabeled_source_mat_ids" = unlabeled_source_mat_ids,
    "min_labeled_sources" = min_labeled_sources,
    "min_unlabeled_sources" = min_unlabeled_sources,
    "min_labeled_fractions" = min_labeled_fractions,
    "min_unlabeled_fractions" = min_unlabeled_fractions
  )

  qsip_data_object@filtered_feature_data <- data |>
    dplyr::filter(feature_id %in% retained_features) |>
    dplyr::select(feature_id, sample_id, tube_rel_abundance) |>
    tidyr::pivot_wider(
      names_from = sample_id,
      values_from = tube_rel_abundance,
      values_fill = 0
    )

  qsip_data_object@filtered_wad_data <- qsip_data_object@wads |>
    dplyr::filter(feature_id %in% retained_features) |>
    dplyr::filter(source_mat_id %in% source_mat_ids) |>
    dplyr::select(feature_id, source_mat_id, WAD) |>
    tidyr::pivot_wider(
      names_from = source_mat_id,
      values_from = WAD
    )

  # update @tube_rel_abundance, @wads, @source_wads and @fraction_counts to only have features/sources that passed the filter

  qsip_data_object@tube_rel_abundance = qsip_data_object@tube_rel_abundance |>
    dplyr::filter(source_mat_id %in% c(qsip_data_object@filter_results$labeled_source_mat_ids, qsip_data_object@filter_results$unlabeled_source_mat_ids)) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features)

  qsip_data_object@wads = qsip_data_object@wads |>
    dplyr::filter(source_mat_id %in% c(qsip_data_object@filter_results$labeled_source_mat_ids, qsip_data_object@filter_results$unlabeled_source_mat_ids)) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features)

  qsip_data_object@source_wads = qsip_data_object@source_wads |>
    dplyr::filter(source_mat_id %in% c(qsip_data_object@filter_results$labeled_source_mat_ids, qsip_data_object@filter_results$unlabeled_source_mat_ids))

  qsip_data_object@fraction_counts = qsip_data_object@fraction_counts |>
    dplyr::filter(source_mat_id %in% c(qsip_data_object@filter_results$labeled_source_mat_ids, qsip_data_object@filter_results$unlabeled_source_mat_ids)) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features)

  return(qsip_data_object)
}




#' Resample WAD values
#'
#' Takes a filtered WAD dataframe and resamples x times
#'
#' This function returns a list of resampled dataframes of x length for both the
#' labeled and unlabeled sources.
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been filtered
#' @param resamples (*integer*) The number of resamples/bootstraps to run
#' @param with_seed (*integer*) An optional seed for reproducibility
#' @param progress (*boolean*) Option to show a progress bar for the resampling step
#' @param allow_failures (*boolean*) Option to allow resampling failures. If TRUE, the function will continue to resample even if some features fail. If FALSE, the function will stop if any features fail.
#' @param quiet (*boolean*) Option to suppress messages
#'
#' @returns A new `qsip_data` object with the `@resamples` slot populated with resamples wad values
#'
#' @export

run_resampling <- function(qsip_data_object,
                           resamples = 1000,
                           with_seed = NULL,
                           allow_failures = FALSE,
                           progress = TRUE,
                           quiet = FALSE) {

  is_qsip_filtered(qsip_data_object, error = TRUE)

  stopifnot("progress must be either TRUE of FALSE" = progress %in% c(TRUE, FALSE))
  stopifnot("resamples should be class <numeric>" = is.numeric(resamples))
  stopifnot("resamples should be positive" = resamples > 0)

  # bind variables
  n <- type <- NULL

  # v0.11.5 fixed to convert to characters
  unlabeled <- as.character(qsip_data_object@filter_results$unlabeled_source_mat_ids)
  labeled <- as.character(qsip_data_object@filter_results$labeled_source_mat_ids)


  # set seed if given
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  } else {
    if (isFALSE(quiet)) {
      message("Using random seed. For consistency, you can use the with_seed argument")
    }
  }

  # create individual dataframes for resampling
  labeled_wads <- qsip_data_object@filtered_wad_data |>
    tibble::column_to_rownames("feature_id") |>
    dplyr::select(dplyr::all_of(labeled))

  unlabeled_wads <- qsip_data_object@filtered_wad_data |>
    tibble::column_to_rownames("feature_id") |>
    dplyr::select(dplyr::all_of(unlabeled))

  # remove NAs?

  # run resampling. This will return a list of x size with resampling results
  if (isTRUE(progress)) {
    progress_bar_l <- "labeled resamples..."
    progress_bar_u <- "unlabeled resamples..."
  } else {
    progress_bar_l <- FALSE
    progress_bar_u <- FALSE
  }

  labeled_resamples <- purrr::map(1:resamples, \(i) calculate_resampled_wads(
    i, labeled_wads,
    "labeled",
    allow_failures = allow_failures
  ), .progress = progress_bar_l)

  unlabeled_resamples <- purrr::map(1:resamples, \(i) calculate_resampled_wads(
    i, unlabeled_wads,
    "unlabeled",
    allow_failures = allow_failures
  ), .progress = progress_bar_u)

  # merge two lists into a list of lists in the @resamples slot
  qsip_data_object@resamples <- list(
    "l" = labeled_resamples,
    "u" = unlabeled_resamples,
    "n" = resamples,
    "seed" = with_seed,
    "allow_failures" = allow_failures
  )

  if (isTRUE(allow_failures)) {
    failures <- get_resample_counts(qsip_data_object) |>
      tidyr::pivot_longer(cols = c("unlabeled_resamples", "labeled_resamples"),
                          names_to = "type",
                          values_to = "n") |>
      dplyr::filter(n < 1000) |>
      dplyr::group_by(type) |>
      dplyr::count() |>
      tibble::deframe()

    if (sum(failures > 0)) {
      if (isFALSE(quiet)) {
        warning(glue::glue("{failures['unlabeled']} unlabeled and {failures['labeled']} labeled feature_ids had resampling failures. Run `get_resample_counts()` or `plot_successful_resamples()` on your <qsip_data> object to inspect."),
                call. = FALSE)
      }
    }
  }

  # return the complete qsip_data object
  qsip_data_object
}





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
  is_qsip_filtered(qsip_data_object, error = TRUE)

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
    dplyr::summarize(W_lab_mean = mean(WAD, na.rm = T))

  observed_unlabeled <- qsip_data_object@wads |>
    dplyr::filter(source_mat_id %in% unlabeled_source_mat_ids) |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features) |>
    dplyr::group_by(feature_id) |>
    dplyr::summarize(W_unlab_mean = mean(WAD, na.rm = T))

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

  is_qsip_data(qsip_data_object, error = TRUE)

  # groups dataframe should contain group, unlabeled and labeled columns, and there can be others
  required_cols <- c("group", "unlabeled", "labeled")
  if (!all(required_cols %in% colnames(groups))) {
    stop(glue::glue("Missing required column names in groups dataframe: {setdiff(required_cols, colnames(groups))}"), call. = F)
  }

  # groups$group column should be unique
  if (length(unique(groups$group)) != nrow(groups)) {
    stop("group column must be unique", call. = FALSE)
  }

  # bind variables
  unlabeled <- labeled <- value <- NULL

  # make sure all source_mat_ids in the isotope columns are in the qsip_data_object
  ## first, fill in "wildcard" source_mat_ids
  groups = groups |>
    dplyr::mutate(unlabeled = dplyr::case_match(
      unlabeled,
      "unlabeled" ~ paste(get_all_by_isotope(qsip_data_object, "unlabeled", quiet = TRUE), collapse = ","),
      "12C" ~ paste(get_all_by_isotope(qsip_data_object, "12C", quiet = TRUE), collapse = ","),
      "14N" ~ paste(get_all_by_isotope(qsip_data_object, "14N", quiet = TRUE), collapse = ","),
      "16O" ~ paste(get_all_by_isotope(qsip_data_object, "16O", quiet = TRUE), collapse = ","),
      .default = unlabeled
    ))  |>
    dplyr::mutate(labeled = dplyr::case_match(
      labeled,
      "labeled" ~ paste(get_all_by_isotope(qsip_data_object, "labeled", quiet = TRUE), collapse = ","),
      "13C" ~ paste(get_all_by_isotope(qsip_data_object, "13C", quiet = TRUE), collapse = ","),
      "15N" ~ paste(get_all_by_isotope(qsip_data_object, "15N", quiet = TRUE), collapse = ","),
      "18O" ~ paste(get_all_by_isotope(qsip_data_object, "18O", quiet = TRUE), collapse = ","),
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
    stop("Invalid source_mat_ids in group dataframe", call. = FALSE)
  }

  group_list <- split(groups, groups$group)

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





#' Run growth calculations
#'
#' @param qsip_data_object (*qsip_data*) An object of `qsip_data` class
#' @param N_total_it (*data.frame*) A data frame of time zero totals from `get_N_total_it()`
#' @param timepoint (*character*) The name of the timepoint column in the source data
#' @param growth_model (*character, default: exponential*) The growth model to use. Must be either "exponential" or "linear"
#' @param correct_copy_numbers (*character, default: filter*) If copy numbers are not logical (e.g. < 0), should they be filtered out or adjusted to 0?
#' @param correct_EAF (*character, default: filter*) If EAF values are not logical (e.g. <0 or >1), should they be filtered out or adjusted to 0 or 1?
#'
#' @export
#'

run_growth_calculations <- function(qsip_data_object,
                                    N_total_it,
                                    growth_model = "exponential",
                                    timepoint = "timepoint",
                                    correct_copy_numbers = "filter",
                                    correct_EAF = "filter") {
  # TODO validate arguments
  # TODO make @timepoint and @total_abundances are not null in @source_data@data. If
  # so, give error saying they must be declared in source_data to get growth

  is_qsip_data(qsip_data_object, error = TRUE)

  # growth_model must be either "exponential" or "linear"
  if (!growth_model %in% c("exponential", "linear")) {
    stop(glue::glue("growth_model must be either 'exponential' or 'linear', not {growth_model}"), call. = FALSE)
  }

  if (!correct_copy_numbers %in% c("filter", "adjust")) {
    stop(glue::glue("correct_copy_numbers must be either 'filter' or 'adjust', not {correct_copy_numbers}"), call. = FALSE)
  }

  if (!correct_EAF %in% c("filter", "adjust")) {
    stop(glue::glue("correct_EAF must be either 'filter' or 'adjust', not {correct_EAF}"), call. = FALSE)
  }

  # error if there is no timepoint column
  if (!timepoint %in% colnames(qsip_data_object@source_data@data)) {
    stop(glue::glue("timepoint column {timepoint} not found in source_data@data"), call. = FALSE)
  }

  # bind variables
  tube_rel_abundance <- feature_id <- source_mat_id <- REL <- total_abundance <- resample <- M <- timepoint1 <- M_heavy <- M_labeled <- N_light_it <- EAF <- N_heavy_it <- N_total_i0 <- di <- bi <- r_net <- ri <- NULL

  time_i_totals <- qsip_data_object@tube_rel_abundance |>
    dplyr::summarize(REL = sum(tube_rel_abundance), .by = c(feature_id, source_mat_id)) |>
    dplyr::left_join(qsip_data_object@source_data@data, by = "source_mat_id") |>
    dplyr::select(
      timepoint = dplyr::all_of(timepoint),
      dplyr::everything()
    ) |>
    dplyr::mutate(normalized_copies = REL * total_abundance) |>
    dplyr::select(feature_id, source_mat_id, timepoint, normalized_copies) |>
    dplyr::filter(source_mat_id %in% qsip_data_object@filter_results$labeled_source_mat_ids) |>
    # TODO line below: should it be mean or sum? They do lead to slightly different results
    dplyr::summarize(
      N_total_it = mean(normalized_copies),
      # timepoint = unique(timepoint), # weird to use unique(), other options?
      .by = c(feature_id, timepoint)
    )



  # merge timepoint i totals with timepoint 0 totals
  normalized_copies <- N_total_it |>
    dplyr::filter(feature_id %in% qsip_data_object@filter_results$retained_features) |>
    dplyr::left_join(time_i_totals, by = "feature_id") |>
    dplyr::mutate(N_total_it = ifelse(is.na(N_total_it), 0, N_total_it))

  # get EAF values for timepoint i
  EAFs <- qsip_data_object@EAF |>
    dplyr::mutate(resample = ifelse(is.na(resample), 0, resample))


  # warn feature_id in normalized_copies not found in EAFs
  no_EAF_values <- normalized_copies |>
    dplyr::filter(!feature_id %in% EAFs$feature_id)

  if (nrow(no_EAF_values) > 0) {
    warning(glue::glue("{nrow(no_EAF_values)} feature_ids have zero abundance at this timepoint and have no EAF values. These values have been filtered out and added to @growth$no_EAF_values"), call. = FALSE)
    qsip_data_object@growth$no_EAF_values <- no_EAF_values
  }

  # merge copy number with EAF values
  rbd <- normalized_copies |>
    dplyr::left_join(EAFs, by = "feature_id") |>
    dplyr::mutate(M_heavy = calculate_M_heavy(propO = qsip_data_object@growth$propO, M)) |>
    dplyr::mutate(time_diff = timepoint - timepoint1) |>
    # overwrite the unlabeled copies with the EAF values
    dplyr::mutate(N_light_it = calculate_N_light_it(N_total_it, M_heavy, M_labeled, M)) |>
    # recalculate N_total_it or labeled using new unlabeled?
    # dplyr::mutate(N_total_it = labeled + unlabeled) |>
    dplyr::mutate(N_heavy_it = N_total_it - N_light_it)

  negative_unlabeled <- rbd |>
    dplyr::filter(N_light_it <= 0 | EAF > 1)

  negative_labeled <- rbd |>
    dplyr::filter(N_heavy_it <= 0 | EAF < 0)

  if (nrow(negative_unlabeled) > 0) {
    warning(glue::glue("{nrow(negative_unlabeled)} calculated values of unlabeled samples are negative. These values have been filtered out and added to @growth$negative_unlabeled"), call. = FALSE)
    qsip_data_object@growth$negative_unlabeled <- negative_unlabeled
  }

  if (nrow(negative_labeled) > 0) {
    warning(glue::glue("{nrow(negative_labeled)} resamplings have a negative EAF value or calculated labeled copy numbers less than 0. These values have been filtered out and added to @growth$negative_labeled"), call. = FALSE)
    qsip_data_object@growth$negative_labeled <- negative_labeled
  }

  if (correct_copy_numbers == "adjust") {
    rbd <- rbd |>
      dplyr::mutate(N_light_it = dplyr::case_when(
        N_light_it < 0 ~ 0,
        TRUE ~ N_light_it
      )) |>
      dplyr::mutate(N_heavy_it = dplyr::case_when(
        N_heavy_it < 0 ~ 0,
        TRUE ~ N_heavy_it
      ))
  }

  if (correct_EAF == "adjust") {
    rbd <- rbd |>
      dplyr::mutate(EAF = dplyr::case_when(
        EAF < 0 ~ 0,
        EAF > 1 ~ 1,
        TRUE ~ EAF
      ))
  }

  # then, working with just the unlabeled that are greater than 0 and have a valid EAF
  rbd <- rbd |>
    dplyr::filter(N_light_it >= 0) |>
    dplyr::filter(N_heavy_it >= 0) |>
    dplyr::filter(EAF >= 0 & EAF <= 1) |>
    dplyr::mutate(
      di = calculate_di(N_light_it, N_total_i0, timepoint, timepoint1, growth_model = growth_model),
      bi = calculate_bi(N_total_it, N_light_it, timepoint, timepoint1, growth_model = growth_model),
      ri = di + bi,
      r_net = N_total_it - N_total_i0
    ) |>
    dplyr::select(feature_id, timepoint1, timepoint2 = timepoint, resample, N_total_i0, N_total_it, N_light_it, N_heavy_it, EAF, r_net, bi, di, ri)

  # mark observed and resamples similar to other qSIP2 objects
  rbd <- rbd |>
    dplyr::mutate(observed = ifelse(resample == 0, TRUE, FALSE)) |>
    dplyr::mutate(resample = ifelse(resample == 0, NA, resample))


  qsip_data_object@growth$rates <- rbd

  return(qsip_data_object)
}



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
