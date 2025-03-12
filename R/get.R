#' Get shared source_mat_ids and sample_ids in qSIP objects
#'
#' @description
#' This function finds the shared source_mat_ids between the `source_data` and
#' `sample_data`, and sample_ids between the `sample_data` and `feature_data` objects.
#' It also reports any ids that are unique to each object.
#'
#' If passing a `qsip_data` object as the first argument, no other arguments are
#' necessary. If a `qsip_source_data` is given, then the `sample_data` and `feature_data`
#' objects must be given as well.
#'
#' Additionally, the results of `get_shared_ids()` might be obtained from the `@shared`
#' slot of a `qsip_data` object. This data can also be "pretty printed" using the
#' `get_unshared_ids(<qsip_data>)` function.
#'
#' @param source_data (*qsip_source_data or qsip_data*) A qSIP object with source data object
#' @param sample_data (*qsip_sample_data*) A qSIP sample data object
#' @param feature_data (*qsip_feature_data*) A qSIP feature data object
#'
#' @export
#'
#' @returns A list with two lists, one for source_mat_ids and one for sample_ids.

get_shared_ids <- function(source_data, sample_data = NULL, feature_data = NULL) {

  # Check that the source_data object is a qsip_source_data or qsip_data object
  stopifnot("source_data should be of class <qsip_source_data> or <qsip_data>" = inherits(source_data, c("qsip_source_data",
                                                                                                         "qSIP2::qsip_source_data",
                                                                                                         "qsip_data",
                                                                                                         "qSIP2::qsip_data")))

  # if passing a complete qsip_data object, extract out the individual components
  if (inherits(source_data, qsip_data)) {
    feature_data <- source_data@feature_data
    sample_data <- source_data@sample_data
    source_data <- source_data@source_data # after getting the first two objects, overwrite the source_data object
  }

  # and make sure these objects are of the correct type, whether passed to the function or created above
  stopifnot("sample_data should be of class <qsip_sample_data>" = inherits(sample_data, qsip_sample_data))
  stopifnot("feature_data should be of class <qsip_feature_data>" = inherits(feature_data, qsip_feature_data))


  # bind variables
  source_mat_id <- sample_id <- feature_id <- NULL

  missing <- FALSE

  # Report the source ids shared and unique
  source_source_mat_id <- source_data@data |>
    dplyr::pull(source_mat_id) |>
    unique()

  sample_source_mat_id <- sample_data@data |>
    dplyr::pull(source_mat_id) |>
    unique()

  shared <- list(
    "source_mat_ids" = list("shared" = c(), "source_data" = c(), "sample_data" = c()),
    "sample_ids" = list("shared" = c(), "sample_data" = c(), "feature_data" = c())
  )

  shared$source_mat_ids$shared <- intersect(source_source_mat_id, sample_source_mat_id)

  if (isTRUE(setequal(source_source_mat_id, sample_source_mat_id))) {
    message(glue::glue_col("{green There are {length(source_source_mat_id)} source_mat_ids, and they are all shared between the source and sample objects}"))
  } else {
    if (length(setdiff(source_source_mat_id, sample_source_mat_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: source_data has {length(setdiff(source_source_mat_id, sample_source_mat_id))} source_mat_id(s) that are missing from the sample_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(source_source_mat_id, sample_source_mat_id), collapse = ', ')}}"))
      shared$source_mat_ids$source_data <- setdiff(source_source_mat_id, sample_source_mat_id)
      missing <- TRUE
    }

    if (length(setdiff(sample_source_mat_id, source_source_mat_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: sample_data has {length(setdiff(sample_source_mat_id, source_source_mat_id))} source_mat_id(s) that are missing from the source_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(sample_source_mat_id, source_source_mat_id), collapse = ', ')}}"))
      shared$source_mat_ids$sample_data <- setdiff(sample_source_mat_id, source_source_mat_id)
      missing <- TRUE
    }
  }

  # Report the sample ids shared and unique
  sample_sample_id <- sample_data@data |>
    dplyr::pull(sample_id) |>
    unique()

  feature_sample_id <- feature_data@data |>
    dplyr::select(-feature_id) |>
    colnames()

  shared$sample_ids$shared <- intersect(sample_sample_id, feature_sample_id)
  if (isTRUE(setequal(sample_sample_id, feature_sample_id))) {
    message(glue::glue_col("{green There are {length(sample_sample_id)} sample_ids, and they are all shared between the sample and feature objects}"))
  } else {
    if (length(setdiff(sample_sample_id, feature_sample_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: sample_data has {length(setdiff(sample_sample_id, feature_sample_id))} sample_id(s) that are missing from the feature_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(sample_sample_id, feature_sample_id), collapse = ', ')}}"))
      shared$sample_ids$sample_data <- setdiff(sample_sample_id, feature_sample_id)
      missing <- TRUE
    }

    if (length(setdiff(feature_sample_id, sample_sample_id) > 0)) {
      message(glue::glue_col("{yellow WARNING: feature_data has {length(setdiff(feature_sample_id, sample_sample_id))} sample_id(s) that are missing from the sample_data}"))
      # message(glue::glue_col("{yellow --> {paste(setdiff(feature_sample_id, sample_sample_id), collapse = ', ')}}"))
      shared$sample_ids$feature_data <- setdiff(feature_sample_id, sample_sample_id)
      missing <- TRUE
    }
  }

  if (isTRUE(missing)) {
    message()
    message(glue::glue_col("{yellow ***Missing source_mat_ids/sample_ids have not been removed from the dataset***}"))
    message(glue::glue_col("{yellow ***Run get_unshared_ids(<qsip_data_object>) to show IDs missing from datasets***}"))  }

  return(shared)
}

#' find_shared_ids (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param source_data (*qsip_source_data or qsip_data*) A qSIP object with source data object
#' @param sample_data (*qsip_sample_data*) A qSIP sample data object
#' @param feature_data (*qsip_feature_data*) A qSIP feature data object
#'
#' @export

find_shared_ids <- function(source_data, sample_data = NULL, feature_data = NULL) {
  lifecycle::deprecate_warn("0.20", "find_shared_ids()", "get_shared_ids()")
  get_shared_ids(source_data, sample_data, feature_data)
}


#' Show missing source_mat_ids and sample_ids
#'
#' @description
#'
#' This function identifies shared and missing source_mat_ids and sample_ids between the source,
#' sample, and feature data objects.
#'
#' @param qsip_data_object (*qsip_data*) A qSIP data object
#'
#' @export
#'
#' @returns A message with unique IDs per category

get_unshared_ids <- function(qsip_data_object) {

  is_qsip_data(qsip_data_object, error = TRUE)

  return(qsip_data_object@shared)
}


#' Show missing source_mat_ids and sample_ids (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param qsip_data_object (*qsip_data*) A qSIP data object
#'
#' @export

show_unshared_ids <- function(qsip_data_object) {
  lifecycle::deprecate_warn("0.20", "show_unshared_ids()", "get_unshared_ids()")
  get_shared_ids(qsip_data_object)
}





#' Get comparison groups
#'
#' Generates a table of ids grouped in columns by isotope, and in rows by the
#' given treatments.
#'
#' @param source_data (*dataframe, qsip_source_data or qsip_data*) Sample metadata
#' @param group (*string*) Treatment value or values
#' @param isotope (*string, default: isotope*) Column name with isotope data
#' @param source_mat_id (*string, default: source_mat_id*) Column name with source_mat_id
#'
#' @export
#'
#' @return A dataframe with id grouped by different `group` treatments and isotopes
#'


get_comparison_groups <- function(source_data = NULL,
                                   group = NULL,
                                   isotope = "isotope",
                                   source_mat_id = "source_mat_id") {

  if (is.null(source_data)) {
    stop("ERROR: Please provide source data with the 'source_data' argument.")
  }

  if (is.null(group)) {
    stop("ERROR: Please provide a grouping variable with the 'group' argument")
  }

  if (is_qsip_data(source_data, error = FALSE)) { # error to false so it continues to else if
    df <- source_data@source_data@data
  } else if (inherits(source_data, qsip_source_data)) {
    df <- source_data@data
  } else if (inherits(source_data, "data.frame")) {
    df <- source_data
  } else {
    class(source_data)
    stop(glue::glue("ERROR: source_data is an unexpected type ({class(source_data)[1]})... it must be class data.frame, qsip_source_data or qsip_data"))
  }

  stopifnot("ERROR: Please provide the column name with the source_mat_id" = source_mat_id %in% colnames(df))
  stopifnot("ERROR: Please provide the column name with isotope data" = isotope %in% colnames(df))

  for (g in group) {
    if (!g %in% colnames(df)) {
      stop(glue::glue("ERROR: grouping column '{g}' not found"))
    }
  }

  # bind variables
  SAMPLES <- NULL

  df |>
    dplyr::select(!!as.name(source_mat_id), !!as.name(isotope), dplyr::all_of(group)) |>
    dplyr::rename(SAMPLES = !!as.name(source_mat_id)) |>
    unique() |>
    tidyr::pivot_wider(
      names_from = !!as.name(isotope),
      values_from = SAMPLES,
      values_fn = list(SAMPLES = ~ paste(., collapse = ", "))
    )
}



#' Show comparison groups (deprecated)
#'
#' Generates a table of ids grouped in columns by isotope, and in rows by the
#' given treatments.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param source_data (*dataframe, qsip_source_data or qsip_data*) Sample metadata
#' @param group (*string*) Treatment value or values
#' @param isotope (*string, default: isotope*) Column name with isotope data
#' @param source_mat_id (*string, default: source_mat_id*) Column name with source_mat_id
#'
#' @export

show_comparison_groups <- function(source_data = NULL,
                                   group = NULL,
                                   isotope = "isotope",
                                   source_mat_id = "source_mat_id") {
  lifecycle::deprecate_warn("0.20", "show_comparison_groups()", "get_comparison_groups()")
  get_comparison_groups(source_data = NULL,
                        group = NULL,
                        isotope = "isotope",
                        source_mat_id = "source_mat_id")
}


#' Get source_mat_ids meeting certain isotope conditions
#'
#' @param qsip_data_object (*qsip_data or qsip_source_data*) A qsip object with source data
#' @param isotopes (*string(s)*) Isotopes used to pull source_mat_ids. Can be a standard isotope name (e.g. `12C`) or special terms `labeled` or `unlabeled`
#' @param quiet (*boolean*) If `TRUE`, suppresses messages about missing isotope hits and doesn't fail
#' @param silent Deprecated, use `quiet` instead
#'
#' @returns A vector of source_mat_ids. It may also print some messages.
#'
#' @export

get_all_by_isotope <- function(qsip_data_object,
                               isotopes,
                               quiet = FALSE,
                               silent = lifecycle::deprecated()) {

  if (lifecycle::is_present(silent)) {
    lifecycle::deprecate_warn("0.18.4.9000", "get_all_by_isotope(silent)")
  }

  if (inherits(qsip_data_object, qsip_data)) {
    source_data <- qsip_data_object@source_data@data
  } else if (inherits(qsip_data_object, qsip_source_data)) {
    source_data <- qsip_data_object@data
  } else {
    stop("qsip_data_object must be class <qsip_data> or <qsip_source_data>")
  }

  if ("labeled" %in% isotopes) {
    isotopes <- c("13C", "15N", "18O")
  } else if ("unlabeled" %in% isotopes) {
    isotopes <- c("12C", "14N", "16O")
  }

  # bind variables
  source_mat_id <- NULL


  # verify given isotopes are valid
  validate_isotopes(isotopes)

  # filter for isotopes
  source_mat_ids <- source_data |>
    dplyr::filter(isotope %in% isotopes) |>
    dplyr::select(source_mat_id, isotope)

  # error if no source_mat_ids are found that match the criteria
  if (isFALSE(quiet)) {

    if (nrow(source_mat_ids) == 0) {
      i <- paste(isotopes, collapse = ", ")
      stop(glue::glue_col("No source_mat_ids found with isotopes {red {i}}"))
    }

    # print a message for each isotope that didn't have any hits. This is FYI and doesn't stop the function
    for (isotope in isotopes) {
      if (!isotope %in% source_mat_ids$isotope) {
        message(glue::glue("WARNING: {isotope} not found in data"))
      }
    }
  }

  # return a list of source_mat_ids
  return(unique(source_mat_ids$source_mat_id))
}






#' Return filtering info for a specific feature ID
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been filtered
#' @param feature_id (*string*) A specific feature_id
#'
#' @export

get_filtered_feature_summary = function(qsip_data_object, feature_id) {

  is_qsip_filtered(qsip_data_object, error = TRUE)

  if (!feature_id %in% get_feature_ids(qsip_data_object)) {
    stop(glue::glue("{feature_id} is not a valid feature_id"), call. = FALSE)
  }

  A = qsip_data_object@filter_results$fraction_filtered |>
    dplyr::filter(feature_id == !!feature_id)
  B = qsip_data_object@filter_results$source_filtered |>
    dplyr::filter(feature_id == !!feature_id)
  C = feature_id %in% qsip_data_object@filter_results$retained_features

  return(list("fraction_filter_summary" = A,
              "source_filter_summary" = B,
              "retained" = C
  ))
}




#' Return the number of sources the feature is found in
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been filtered
#'
#' @export

get_filtered_source_counts = function(qsip_data_object) {

  is_qsip_filtered(qsip_data_object, error = TRUE)

  # bind variables
  fraction_call <- feature_id <- type <- counts <- labeled <- unlabeled <- NULL

  qsip_data_object@filter_results$fraction_filtered |>
    dplyr::filter(fraction_call == "Fraction Passed") |>
    dplyr::group_by(feature_id, type) |>
    dplyr::count(name = "counts") |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = type, values_from = counts) |>
    dplyr::rename("unlabeled_sources" = unlabeled,
                  "labeled_sources" = labeled)

}


#' Get dataframe of resampled growth data
#'
#' This function will return a dataframe of the resampled growth data embedded in a `qsip_data`
#' object.
#'
#' @param qsip_data_object A `qsip_data` object that has been run through `run_growth_calculations()`
#'
#' @export
#'
#' @returns A dataframe of the resampled data

get_growth_data <- function(qsip_data_object) {
  is_qsip_growth(qsip_data_object, error = TRUE)

  resample <- NULL

  # bind variables
  observed <- NULL

  qsip_data_object@growth$rates |>
    dplyr::select(-observed) |>
    dplyr::filter(!is.na(resample))

}






#' Infer which isotope calculations to use (internal)
#'
#' This uses the list of the source_mat_ids in a filtered qsip_data object to get
#' their isotope designation for two reasons, 1) to make sure they are comparable
#' (e.g. are all 12C/13C, 14N/15N or 16O/18O with no mismatches), and 2) to return
#' the best guess of the labeled isotope so EAF calculations will proceed correctly.
#'
#' As of v0.15.2 it is now possible to have a mismatch. This is particularly
#' important with multiple isotope studies where you may want to compare a 13C sample
#' to a 12C sample, but also an 18O against the same 12C sample.
#'
#' @param qsip_data_object (*qsip_data*) A `qsip_data` object
#' @param unlabeled_source_mat_ids (*character*) A vector of source_mat_ids that are unlabeled
#' @param labeled_source_mat_ids (*character*) A vector of source_mat_ids that are labeled
#'
#' @returns A single labeled isotope designation of 13C, 15N or 18O, and gives an
#' error if an inference cannot be made.


get_isotope_designation <- function(qsip_data_object, unlabeled_source_mat_ids, labeled_source_mat_ids) {

  is_qsip_data(qsip_data_object, error = TRUE)

  # check unlabeled_source_mat_ids is a character vector with a size of 1 or more
  if (!is.character(unlabeled_source_mat_ids) | length(unlabeled_source_mat_ids) == 0) {
    stop("unlabeled_source_mat_ids must be a character vector with a size of 1 or more")
  }

  # check labeled_source_mat_ids is a character vector with a size of 1 or more
  if (!is.character(labeled_source_mat_ids) | length(labeled_source_mat_ids) == 0) {
    stop("labeled_source_mat_ids must be a character vector with a size of 1 or more")
  }

  # bind variables
  source_mat_id <- isotope <- NULL

  unlabeled_isotopes = qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% unlabeled_source_mat_ids) |>
    dplyr::pull(isotope) |>
    unique()
  validate_isotopes(unlabeled_isotopes, c("12C", "14N", "16O"))

  labeled_isotopes <- qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% labeled_source_mat_ids) |>
    dplyr::pull(isotope) |>
    unique()
  validate_isotopes(labeled_isotopes, c("13C", "15N", "18O"))

  if (length(unique(labeled_isotopes)) == 1) {
    return(unique(labeled_isotopes))
  } else {
    stop("There is a mixture of multiple labeled isotopes in this group, which is not allowed")
  }
}



#' Get counts of successful resampling
#'
#' For each feature_id and label type, this function will return the counts of successful
#' resampling. This value will typically be the number of resamples given to `run_resampling()`,
#' but if `run_resampling()` is called with `allow_failures = TRUE` then the number of
#' successful resamples might be less than the number of resamples given.
#'
#' If as_percentage is TRUE, the counts will be returned as a percentage of
#' the total number of resamples.
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been resampled
#' @param as_percentage (*logical*) If TRUE, the counts will be returned as a percentage of the total number of resamples
#'
#'
#' @export
#'
#' @returns A dataframe with columns feature_id, type, and n

get_resample_counts = function(qsip_data_object,
                               as_percentage = FALSE) {

  is_qsip_resampled(qsip_data_object, error = TRUE)

  # bind variables
  feature_id <- type <- n_counts <- labeled <- unlabeled <- NULL

  u = dplyr::bind_rows(qsip_data_object@resamples$u, .id = "resample")
  u = u[rowSums(is.na(u)) != ncol(u) - 3, ] |>
    dplyr::select(feature_id, type)

  l = dplyr::bind_rows(qsip_data_object@resamples$l, .id = "resample")
  l = l[rowSums(is.na(l)) != ncol(l) - 3, ] |>
    dplyr::select(feature_id, type)

  counts = rbind(u, l) |>
    dplyr::group_by(feature_id, type) |>
    dplyr::count(name = "n_counts") |>
    dplyr::ungroup()

  if (isTRUE(as_percentage)) {
    counts = counts |>
      dplyr::mutate(n_counts = n_counts / qsip_data_object@resamples$n)
  }

  counts = counts |>
    tidyr::pivot_wider(names_from = "type", values_from = "n_counts") |>
    dplyr::rename("labeled_resamples" = labeled,
                  "unlabeled_resamples" = unlabeled)


  return(counts)
}






#' Get dataframe of resampled data
#'
#' This function will return a dataframe of the resampled data embedded in a `qsip_data`
#' object.
#'
#' @param qsip_data_object A `qsip_data` object that has been run through `run_resampling()`
#' @param type (*string*) The type of data to return: "all", "unlabeled", or "labeled"
#' @param pivot (*boolean*) Whether to pivot the data into a long format or keep as wide
#'
#' @export
#'
#' @returns A dataframe of the resampled data


get_resample_data <- function(qsip_data_object,
                              type = "all",
                              pivot = FALSE) {
  if (isFALSE(is_qsip_resampled(qsip_data_object, error = FALSE))) {
    stop("This function requires a qsip object that has been run through run_resampling()", call. = FALSE)
  }

  # error if type is not one of "all", "unlabeled", or "labeled"
  if (!type %in% c("all", "unlabeled", "labeled")) {
    stop("type must be one of 'all', 'unlabeled', or 'labeled'", call. = FALSE)
  }

  # error if pivot is not TRUE/FALSE
  if (!isTRUE(pivot) && !isFALSE(pivot)) {
    stop("pivot must be TRUE or FALSE", call. = FALSE)
  }


  # bind variables
  feature_id <- resample <- NULL

  if (type == "all") {
    u <- dplyr::bind_rows(qsip_data_object@resamples$u) |> dplyr::select(-type)
    l <- dplyr::bind_rows(qsip_data_object@resamples$l) |> dplyr::select(-type)
    df <- dplyr::left_join(u, l, by = dplyr::join_by(feature_id, resample))
  } else if (type == "unlabeled") {
    df <- dplyr::bind_rows(qsip_data_object@resamples$u) |> dplyr::select(-type)
  } else if (type == "labeled") {
    df <- dplyr::bind_rows(qsip_data_object@resamples$l) |> dplyr::select(-type)
  }


  if (isTRUE(pivot)) {
    df |>
      tidyr::pivot_longer(cols = c(tidyr::starts_with("unlabeled"), tidyr::starts_with("labeled_")), names_to = "type", values_to = "WAD") |>
      tidyr::separate(type, into = c("type", "replicate"), sep = "_")
  } else {
    df
  }
}






#' Return the feature_ids in a qsip object
#'
#' @param qsip_data_object A `qSIP_data` object that has been run through `run_resampling()`
#' @param filtered (*Boolean*) If TRUE, return the feature_ids from the filtered data
#'
#' @export

get_feature_ids = function(qsip_data_object,
                           filtered = FALSE) {

  is_qsip_data(qsip_data_object, error = TRUE)

  # filtered must be a boolean
  if (!is.logical(filtered)) {
    stop("<filtered> must be TRUE/FALSE", call. = FALSE)
  }

  if (filtered) {

    if (is_qsip_filtered(qsip_data_object, error = FALSE)) {
      return(qsip_data_object@filter_results$retained_features)
    } else {
      stop("No filtered feature_ids in this <qsip_data> object. Try filtering first, or setting the <filtered> to FALSE", call. = FALSE)
    }
  } else {
    return(qsip_data_object@feature_data@data$feature_id)
  }

}






#' Return the source_mat_ids in a qsip object
#'
#' @param qsip_data_object A `qSIP_data` object that has been run through `run_resampling()`
#' @param filtered (*Boolean*) If TRUE, return the feature_ids from the filtered data
#'
#' @export

get_source_mat_ids = function(qsip_data_object,
                              filtered = FALSE) {

  is_qsip_data(qsip_data_object, error = TRUE)

  # filtered must be a boolean
  if (!is.logical(filtered)) {
    stop("<filtered> must be TRUE/FALSE", call. = FALSE)
  }

  if (filtered) {

    if (is_qsip_filtered(qsip_data_object, error = FALSE)) {

      source_mat_ids = c(qsip_data_object@filter_results$unlabeled_source_mat_ids,
                         qsip_data_object@filter_results$labeled_source_mat_ids)

      return(source_mat_ids)
    } else {
      stop("No filtered source_mat_ids in this <qsip_data> object. Try filtering first, or setting the <filtered> to FALSE", call. = FALSE)
    }
  } else {
    return(qsip_data_object@source_data@data$source_mat_id)
  }

}




#' Number of resamples
#'
#' This function returns the number of resamples that were performed on the data.
#' It will return `NULL` if the data has not been resampled.
#'
#' @param qsip_data_object A `qSIP_data` object or list of objects
#'
#' @export

n_resamples <- function(qsip_data_object) {
  if (is_qsip_data(qsip_data_object)) {
    resamples = qsip_data_object@resamples$n
    return(resamples)
  } else if (is_qsip_data_list(qsip_data_object)) {
    resamples = lapply(qsip_data_object, function(x) {x@resamples$n}) |>
      unlist() |>
      tibble::enframe(name = "group", value = "n_resamples")
    return(resamples)
  } else {
    stop("this function requires a <qsip_data> object, or a list of <qsip_data> objects", call. = FALSE)
  }
}



#' Seed used in resampling
#'
#' Returns the seed used in the resampling step, or `NULL` if no specific seed was given.
#'
#' @param qsip_data_object A `qSIP_data` or list of `qSIP_data` objects.
#'
#' @export

resample_seed <- function(qsip_data_object) {
  if (is_qsip_data(qsip_data_object)) {
    seed_used = qsip_data_object@resamples$seed
    return(seed_used)
  } else if (is_qsip_data_list(qsip_data_object)) {
    seed_used = lapply(qsip_data_object, function(x) {x@resamples$seed}) |>
      unlist() |>
      tibble::enframe(name = "group", value = "seed")
    return(seed_used)
  } else {
    stop("this function requires a <qsip_data> object, or a list of <qsip_data> objects", call. = FALSE)
  }
}



#' Filter features by fraction message formatting (internal)
#'
#' @param by_fraction by_fraction dataframe from run_feature_filter
#'
#' @keywords internal

fraction_results_message <- function(by_fraction) {

  # binding variables
  feature_id <- type <- fraction_call <- counts <- NULL

  fraction_results <- by_fraction |>
    dplyr::count(feature_id, type, fraction_call) |>
    # unique() |>
    dplyr::count(type, fraction_call, name = "counts") |>
    tidyr::pivot_wider(names_from = type, values_from = counts, values_fill = 0) |>
    tibble::column_to_rownames("fraction_call")

  if (!is.na(fraction_results["Zero Fractions", "unlabeled"])) {
    message(glue::glue_col('{red {fraction_results["Zero Fractions","unlabeled"]}} unlabeled and {red {fraction_results["Zero Fractions","labeled"]}} labeled feature_ids were found in {red zero fractions} in at least one source_mat_id'))
  }

  if (!is.na(fraction_results["Fraction Filtered", "unlabeled"])) {
    message(glue::glue_col('{red {fraction_results["Fraction Filtered","unlabeled"]}} unlabeled and {red {fraction_results["Fraction Filtered","labeled"]}} labeled feature_ids were found in {red too few fractions} in at least one source_mat_id'))
  }

  if (!is.na(fraction_results["Fraction Passed", "unlabeled"])) {
    message(glue::glue_col('{green {fraction_results["Fraction Passed","unlabeled"]}} unlabeled and {green {fraction_results["Fraction Passed","labeled"]}} labeled feature_ids {green passed} the fraction filter'))
  }

  fraction_passed <- by_fraction |>
    dplyr::filter(fraction_call == "Fraction Passed") |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()

  message(glue::glue_col("In total, {green {fraction_passed}} unique feature_ids {green passed} the fraction filtering requirements..."))

}





#' Filter features by source message formatting (internal)
#'
#' @param by_source by_source dataframe from run_feature_filter
#'
#' @keywords internal

source_results_message <- function(by_source) {

  # binding variables
  feature_id <- type <- source_call <- counts <- labeled <- unlabeled <- NULL

  source_results <- by_source |>
    dplyr::select(feature_id, type, source_call) |>
    unique() |>
    dplyr::count(type, source_call, name = "counts") |>
    tidyr::pivot_wider(names_from = type, values_from = counts, values_fill = 0) |>
    tibble::column_to_rownames("source_call")

  if (!is.na(source_results["Zero Sources", "unlabeled"])) {
    message(glue::glue_col('{red {source_results["Zero Sources","unlabeled"]}} unlabeled and {red {source_results["Zero Sources","labeled"]}} labeled feature_ids failed the source filter because they were found in {red zero sources}'))
  }

  if (!is.na(source_results["Source Filtered", "unlabeled"])) {
    message(glue::glue_col('{red {source_results["Source Filtered","unlabeled"]}} unlabeled and {red {source_results["Source Filtered","labeled"]}} labeled feature_ids failed the source filter because they were found in {red too few sources}'))
  }

  if (!is.na(source_results["Source Passed", "unlabeled"])) {
    message(glue::glue_col('{green {source_results["Source Passed","unlabeled"]}} unlabeled and {green {source_results["Source Passed","labeled"]}} labeled feature_ids {green passed} the source filter'))
  }

  total_passed <- by_source |>
    dplyr::select(feature_id, type, source_call) |>
    tidyr::pivot_wider(names_from = type, values_from = source_call) |>
    dplyr::filter(labeled == "Source Passed" & unlabeled == "Source Passed") |>
    dplyr::pull(feature_id) |>
    unique() |>
    length()

  message(rep("=+", 25))

  message(glue::glue_col("In total, {green {total_passed}} unique feature_ids {green passed} all fraction and source filtering requirements"))
}




#' Calculate total abundances at timepoint t
#'
#' This function takes a `qsip_data` object and calculates the total abundance of
#' each feature at time zero. This should be done on an early `qsip_data` object
#' that still has time zero data.
#'
#' Sometimes, a feature will have abundance at a later time point, but no values
#' for time zero. If a feature has zero abundance at time zero, a warning will
#' be issued, but the feature will still be included in the output with a starting
#' abundance of zero.
#'
#' @param qsip_data_object (*qsip_data*) An object of `qsip_data` class
#' @param timepoint (*character*) The name of the timepoint column in the source data
#' @param t (*numeric*) The value of the timepoint column to filter on
#' @param group (*character*) The name of the grouping variable(s) to summarize the counts
#'
#' @export
#'
#' @returns (*data.frame*) A data frame with feature_id and total abundance at time zero

get_N_total_it <- function(qsip_data_object,
                           timepoint = "timepoint",
                           t = 0,
                           group = NULL) {

  is_qsip_data(qsip_data_object, error = TRUE)

  # stop if t is not numeric
  if (!is.numeric(t)) {
    stop("t must be numeric", call. = FALSE)
  }

  # bind variables
  tube_rel_abundance <- feature_id <- source_mat_id <- REL <- total_abundance <- zero <- NULL

  # this section makes the basic dataframe with total abundance at time zero.
  # Because it is based on @tube_rel_abundance it will not have data for
  # features with zero abundance at time zero
  N_total_i0 <- qsip_data_object@tube_rel_abundance |>
    dplyr::summarize(REL = sum(tube_rel_abundance), .by = c(feature_id, source_mat_id)) |>
    dplyr::left_join(qsip_data_object@source_data@data, by = "source_mat_id") |>
    dplyr::select(
      timepoint = dplyr::all_of(timepoint),
      dplyr::everything()
    )

  # make sure there are values of t in the dataframe
  if (!t %in% N_total_i0$timepoint) {
    stop(glue::glue("no source_mat_ids with a 't = {t}' timepoint were found in <qsip_data>"), call. = FALSE)
  }

  # group can be NULL, or should be a column in N_total_i0
  if (!is.null(group)) {

    if (!group %in% colnames(N_total_i0)) {
      stop(glue::glue("grouping variable {group} not found in source_data"), call. = FALSE)
    }
  }



  N_total_i0 = N_total_i0 |>
    dplyr::filter(timepoint == t) |>
    dplyr::mutate(N_total_i0 = REL * total_abundance)
  # dplyr::select(feature_id, source_mat_id, timepoint, N_total_i0) |>
  # TODO line below: should it be mean or sum?
  # TODO should this be filtered to only include unlabeled? Timepoints other than 0 might have labeled samples

  # if using a grouping variable(s) to summarize the counts
  # !!! : https://stackoverflow.com/questions/42612417/how-to-pass-multiple-column-names-as-input-to-group-by-in-dplyr/42612631
  # sym : https://stackoverflow.com/questions/61180201/triple-exclamation-marks-on-r
  if (isFALSE(is.null(group))) {
    N_total_i0 = N_total_i0 |>
      dplyr::group_by(feature_id, !!!dplyr::syms(group)) |>
      dplyr::summarize(N_total_i0 = mean(N_total_i0), .groups = "drop")
  } else {
    N_total_i0 = N_total_i0 |>
      dplyr::summarize(N_total_i0 = mean(N_total_i0), .by = feature_id)
  }

  # make dataframe with zeroes. This ensures feature_ids with zero abundance
  # will still be present in the resulting dataframe
  N_total_i0 <- get_dataframe(qsip_data_object, type = "feature") |>
    dplyr::select(feature_id) |>
    dplyr::mutate(zero = 0) |>
    dplyr::left_join(N_total_i0, by = "feature_id") |>
    dplyr::group_by(feature_id, !!!dplyr::syms(group)) |>
    dplyr::mutate(N_total_i0 = sum(zero, N_total_i0, na.rm = T)) |>
    dplyr::select(-zero) |>
    dplyr::ungroup()

  # report on zero abundance samples
  no_abundance <- N_total_i0 |>
    dplyr::filter(N_total_i0 == 0)

  if (nrow(no_abundance) > 0) {
    warning(glue::glue("{nrow(no_abundance)} feature_ids have zero abundance at time {t}:"), call. = F)
    warning(paste(no_abundance$feature_id, collapse = ", "), call. = F)
  }


  # add timepoint as timepoint1
  N_total_i0 <- N_total_i0 |>
    dplyr::mutate(timepoint1 = t)

  return(N_total_i0)
}





#' Summarize growth values
#'
#' @param qsip_data_object A qsip_data object
#' @param confidence (*numeric, default: 0.9*) The confidence level for the growth values
#' @param quiet (*logical, default: FALSE*) Suppress messages
#'
#' @export

summarize_growth_values <- function(qsip_data_object, confidence = 0.9, quiet = FALSE) {
  # confirm the data is the correct type
  is_qsip_growth(qsip_data_object, error = TRUE)

  # confirm the confidence value is numeric and between 0-1
  stopifnot("ERROR: confidence should be numeric" = is.numeric(confidence))
  if (confidence >= 1 | confidence <= 0) {
    stop("ERROR: confidence level should be between 0 and 1")
  }

  observed <- feature_id <- timepoint1 <- timepoint2 <- N_total_i0 <- N_total_it <- r_net <- bi <- di <- ri <- EAF <- desc <- observed_ri <- NULL


  if (isFALSE(quiet)) {
    message(glue::glue("Confidence level = {confidence}"))
  }

  rbd_observed <- qsip_data_object@growth$rates |>
    dplyr::filter(observed == TRUE) |>
    dplyr::select(feature_id,
                  timepoint1,
                  timepoint2,
                  N_total_i0,
                  N_total_it,
                  r_net,
                  observed_bi = bi,
                  observed_di = di,
                  observed_ri = ri
    )

  rbd_resamples <- qsip_data_object@growth$rates |>
    dplyr::filter(observed == FALSE) |>
    dplyr::select(-observed) |>
    dplyr::summarize(
      successes = dplyr::n(),
      resampled_N_mean = mean(N_total_it, na.rm = TRUE),
      # resampled_N_sd = sd(N_total_it, na.rm = TRUE),
      # resampled_N_lower = quantile(N_total_it, (1 - confidence) / 2, na.rm = T),
      # resampled_N_upper = quantile(N_total_it, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_rnet_mean = mean(r_net, na.rm = TRUE),
      # resampled_rnet_sd = sd(r_net, na.rm = TRUE),
      # resampled_rnet_lower = quantile(r_net, (1 - confidence) / 2, na.rm = T),
      # resampled_rnet_upper = quantile(r_net, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_bi_mean = mean(bi, na.rm = TRUE),
      resampled_bi_sd = sd(bi, na.rm = TRUE),
      resampled_bi_lower = quantile(bi, (1 - confidence) / 2, na.rm = T),
      resampled_bi_upper = quantile(bi, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_di_mean = mean(di, na.rm = TRUE),
      resampled_di_sd = sd(di, na.rm = TRUE),
      resampled_di_lower = quantile(di, (1 - confidence) / 2, na.rm = T),
      resampled_di_upper = quantile(di, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_ri_mean = mean(ri, na.rm = TRUE),
      resampled_ri_sd = sd(ri, na.rm = TRUE),
      resampled_ri_lower = quantile(ri, (1 - confidence) / 2, na.rm = T),
      resampled_ri_upper = quantile(ri, 1 - (1 - confidence) / 2, na.rm = T),
      resampled_EAF_mean = mean(EAF, na.rm = TRUE),
      resampled_EAF_sd = sd(EAF, na.rm = TRUE),
      resampled_EAF_lower = quantile(EAF, (1 - confidence) / 2, na.rm = T),
      resampled_EAF_upper = quantile(EAF, 1 - (1 - confidence) / 2, na.rm = T),
      .by = feature_id
    )


  rbd <- rbd_observed |>
    dplyr::left_join(rbd_resamples, by = "feature_id") |>
    dplyr::arrange(desc(observed_ri))

  # TODO make some sort of note about infinite values

  return(rbd)
}





#' Summarize the observed and resampled EAF values
#'
#' Reports observed EAF value (`observed_EAF`) as well as the mean of the resampled
#' values (`mean_resampled_EAF`) and the `lower` and `upper` confidence interval
#' with a given `confidence` limit.
#'
#' The confidence interval uses the resampling method where it returns the quantile
#' values from the resampled data. If `confidence = 0.9` (the default) then this
#' function returns the 5% and 95% quantiles (representing 90% of the resampling)
#' as the `lower` and `upper` results.
#'
#' @param qsip_data_object (*qsip_data*) A qsip_data object or list of qsip_data objects
#' @param confidence (*numeric, default: 0.9*) The confidence level for the confidence interval
#' @param quiet (*logical, default: FALSE*) Suppress messages
#'
#' @export
#'
#' @returns A `dataframe` with summarized observed and resampled EAF values

summarize_EAF_values <- function(qsip_data_object, confidence = 0.9, quiet = FALSE) {
  # confirm the confidence value is numeric and between 0-1
  stopifnot("ERROR: confidence should be numeric" = is.numeric(confidence))
  if (confidence >= 1 | confidence <= 0) {
    stop("ERROR: confidence level should be between 0 and 1")
  }

  if (isFALSE(quiet)) {
    message(glue::glue("Confidence level = {confidence}"))
  }

  # confirm qsip_data_object class is either qsip_data or list
  if (is_qsip_data_list(qsip_data_object, error = FALSE)) {
    lapply(qsip_data_object,
           summarize_EAF_values_internal,
           confidence = confidence
    ) |>
      dplyr::bind_rows(.id = "group")
  } else if (is_qsip_data(qsip_data_object, error = FALSE)) {
    summarize_EAF_values_internal(qsip_data_object,
                                  confidence = confidence)
  } else {
    stop("ERROR: qsip_data_object must be of class <qsip_data> or <list> of qsip_data objects")
  }
}





#' Internal function to summarize EAF values
#'
#' @param qsip_data_object (*qsip_data*) A qsip_data object
#' @param confidence (*numeric, default: 0.9*) The confidence level for the confidence interval
#'
#' Called by `summarize_EAF_values` to calculate the resampled EAF values.
#'
#' @keywords internal

summarize_EAF_values_internal <- function(qsip_data_object,
                                          confidence = 0.9) {

  is_qsip_filtered(qsip_data_object, error = TRUE)

  feature_id <- EAF <- NULL

  resamples <- qsip_data_object@EAF |>
    dplyr::filter(observed == FALSE) |>
    dplyr::group_by(feature_id) |>
    dplyr::summarize(
      mean_resampled_EAF = mean(EAF),
      lower = quantile(EAF, (1 - confidence) / 2, na.rm = T),
      upper = quantile(EAF, 1 - (1 - confidence) / 2, na.rm = T),
      .groups = "drop"
    )

  observed <- qsip_data_object@EAF |>
    dplyr::filter(observed == TRUE) |>
    dplyr::select(feature_id, observed_EAF = EAF)

  observed |>
    dplyr::left_join(resamples, by = "feature_id") |>
    dplyr::left_join(get_resample_counts(qsip_data_object), by = "feature_id") |>
    dplyr::left_join(get_filtered_source_counts(qsip_data_object), by = "feature_id")
}
