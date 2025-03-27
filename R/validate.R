#' Check object is qsip_data type
#'
#' @param object The object to check if it is a qsip_data object
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_data = function(object, error = FALSE) {

  # error must be a logical
  if (!is.logical(error)) {
    stop("error must be a logical", call. = FALSE)
  }

  if (!inherits(object, qsip_data)) {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("object must be a <qsip_data> object, not <{class(object)[1]}>"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}


#' Validate a multi-qsip list object
#'
#' @param object A list of qsip_data objects
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_data_list <- function(object, error = FALSE) {

  # check if object is a list
  if(!is.list(object)) {
    if (isTRUE(error)) {
      stop("object must be a list")
    } else {
      return(FALSE)
    }
  }

  is_qsip_data_results = lapply(object, is_qsip_data)

  if (isTRUE(all(unlist(is_qsip_data_results)))) {
    return(TRUE)
  } else {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("object must be a <qsip_data> object, not <{class(object)[1]}>"), call. = FALSE)
    }
  }
}



#' Validate a qsip object has been filtered
#'
#' @param object The object to check if it is a filtered qsip_data object
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_filtered <- function(object, error = FALSE) {

  # first check if it is even a qsip object
  is_qsip_data(object, error = TRUE)

  if (length(object@filtered_wad_data) == 0) {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("object is a non-filtered <qsip_data> object"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}



#' Validate a qsip object has been resampled
#'
#' @param object The object to check if it is a resampled qsip_data object
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_resampled <- function(object, error = FALSE) {

  # first check if it is even a qsip object
  is_qsip_data(object, error = TRUE)

  if (length(object@resamples) == 0) {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("object is a non-resampled <qsip_data> object"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}



#' Validate a qsip object has EAF values
#'
#' @param object The object to check if it is a qsip_data object with EAF calculations
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_EAF <- function(object, error = FALSE) {

  # first check if it is even a qsip object
  is_qsip_data(object, error = TRUE)

  if (!dim(object@EAF)[1] > 0) {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("<qsip_data> object does not have EAF calculations"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}



#' Validate a qsip object has been run through growth workflow
#'
#' @param object The object to check if it is a growth qsip_data object
#' @param error If TRUE it stops with an error message. If FALSE it doesn't error, but returns FALSE
#'
#' @export

is_qsip_growth <- function(object, error = FALSE) {

  # first check if it is even a qsip object
  is_qsip_data(object, error = TRUE)

  if (is.null(object@growth$rates))  {
    if (isFALSE(error)) {
      return(FALSE)
    } else {
      stop(glue::glue("<object> has not been run through the growth calculations"), call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}



#' Check the validity of a feature abundance table (internal)
#'
#' This validation function is an internal function will check the validity of a
#' feature abundance table.
#'
#' Rows should contain the unique taxa ids with a column designated with the id
#' argument. Each other column name should be a unique sample name.
#'
#' Validity checking includes making sure all data is numeric (except for the
#' feature IDs), all numbers are integers (if `type = "counts"`), and no numbers are
#' negative.
#'
#' @param data (*dataframe*) ASV/OTU table
#' @param feature_id (*string*) Column name with feature IDs
#' @param type (*string*) *counts* requires integers, *coverage* and *relative* can take any positive numeric
#'
#' @returns Returns `NULL` if the values are valid, or a printed error
#'
#' @keywords internal
#' @export


validate_abundances <- function(data,
                                feature_id,
                                type) {
  stopifnot("feature data type should be 'counts', 'coverage', 'normalized' or 'relative'" =  type %in% c("counts", "coverage", "normalized", "relative"))

  name <- value <- S <- NULL

  if (type == "relative") {
    totals <- data |>
      tidyr::pivot_longer(cols = dplyr::where(is.numeric)) |>
      dplyr::group_by(name) |>
      dplyr::summarise(S = sum(value)) |>
      dplyr::filter(S > 1.001) |>
      dplyr::pull(name) |>
      length()

    if (totals > 0) {
      stop("Some columns have a total relative abundance sum greater than 1", call. = FALSE)
    }
  }

  values <- data |>
    dplyr::select(-dplyr::all_of(feature_id))

  if (any(values < 0)) {
    stop("Some numbers are negative", call. = FALSE)

  } else if (!all(values - floor(values) == 0)) {
    if (type == "counts") {
      stop("Some data are not integers", call. = FALSE)
    }

    # this was in here as a check, but it isn't clear when some data might pass
    # the integer check above, but then still not be numeric.
    # So, commenting out for now.
    # } else if (length(values) - length(dplyr::select_if(values, is.numeric)) > 0) {
    #   stop("Some data is not numeric", call. = FALSE)

  } else {
    return(NULL)
  }
}





#' Check the validity of density values (internal)
#'
#' @param df (*dataframe*) A two-column dataframe with density positions and density values
#' @param high (*numeric, default: 1.8*) A high limit for valid density values
#' @param low (*numeric, default: 1.55*) A low limit for valid density values
#'
#' @returns Returns `NULL` if the density values are valid, or a printed error
#'
#' @keywords internal

validate_gradient_pos_density <- function(df, low = 1.55, high = 1.8) {

  stopifnot("data should be class <data.frame>" = "data.frame" %in% class(df))

  gradient_position <- NULL

  # message if any fractions are -1 indicating bulk data. These will be ignored
  # from the validation because there densities are often NA
  if(any(df$gradient_position == -1)) {
    #message("some samples have a gradient_position of -1 and their gradient_pos_density will not be validated")
    gradient_pos_density = df |>
      dplyr::filter(gradient_position > 0) |>
      dplyr::pull(gradient_pos_density)
  } else{
    gradient_pos_density = df |>
      dplyr::pull(gradient_pos_density)
  }

  stopifnot("some gradient_pos_density values are non-numeric" = is.numeric(gradient_pos_density))

  if (any(gradient_pos_density > high)) {
    stop(glue::glue("some gradient_pos_density values are higher than {high}"))
  } else if (any(gradient_pos_density < low)) {
    stop(glue::glue("some gradient_pos_density values are lower than {low}"))
  } else {
    return(NULL)
  }

}




#' Check the validity of gradient position values (internal)
#'
#' Valid gradient positions are integers. The value can be `-1` to represent
#' "bulk" or non-qSIP samples.
#'
#' @param gradient_position (*string or strings*) Gradient position value or values
#'
#' @returns `NULL` if the gradient position values are valid, or a
#' printed error
#'
#' @keywords internal

validate_gradient_position <- function(gradient_position) {
  if (is.numeric(gradient_position)) {
    # https://www.tutorialspoint.com/how-to-check-if-all-values-in-a-vector-are-integer-or-not-in-r
    if (all(gradient_position - floor(gradient_position) == 0)) {
      return(NULL)
    } else {
      stop("some gradient_position values are not integers", call. = FALSE)
    }
  } else {
    stop("some gradient_position values are non-numeric", call. = FALSE)
  }
}




#' Check the validity of an isotope string (internal)
#'
#' Often the "bulk" designation is found in this column, so those can optionally
#' be removed from validation checks
#'
#' @param isotope (*string(s)*) Isotope value or values
#' @param isotope_list (*strings, default: c("12C", "13C", "14N", "15N", "16O", "18O")*) Isotopes to check against
#' @param unfractionated_terms (*strings*) Terms to ignore when checking isotope values
#'
#' @returns Returns `NULL` if the isotope strings are valid, or a printed error
#'
#' @keywords internal
#'
#' @note The isotope_list may change if isotopolog_label stays a thing. Only the "labeled" isotopes will be allowed.

validate_isotopes <- function(isotope,
                              isotope_list = c(valid_isotope_names$unlabeled, valid_isotope_names$labeled),
                              unfractionated_terms = valid_isotope_names$unfractionated) {

  # if any unfractionated terms are found, print a message
  if (any(unfractionated_terms %in% isotope)) {
    for (term in intersect(isotope, unfractionated_terms)) {
      message(glue::glue("Isotope value found that matches typical unfractionated terms: {term}"))
    }
  }

  # remove unfractionated terms from isotope
  isotope = isotope[!isotope %in% unfractionated_terms]

  if (length(setdiff(isotope, isotope_list)) == 0) {
    return(NULL)
  } else {
    for (error in setdiff(isotope, isotope_list)) {
      message(glue::glue("invalid isotope found: {error}"))
    }
    stop("Please fix the isotope names and try again", call. = FALSE)
  }
}


#' Validate the given source mat ids have the expected labeled/unlabeled designation (internal)
#'
#' Currently used by `run_feature_filter()` to make sure user given labeled or unlabeled
#' source_mat_ids are not incorrect with respective to their source_data.
#'
#' @param qsip_data_object A qsip_data object
#' @param source_mat_ids A character vector of source_mat_ids
#' @param isotope_list A character vector of isotopes to check against
#'
#' @keywords internal
#'
#' @returns TRUE (all match) or FALSE (some don't match)

validate_source_isotope <- function(qsip_data_object, source_mat_ids, isotope_list) {
  source_mat_id <- isotope <- NULL

  source_isotopes <- qsip_data_object@source_data@data |>
    dplyr::filter(source_mat_id %in% source_mat_ids) |>
    dplyr::pull(isotope) |>
    unique()

  if (length(setdiff(source_isotopes, isotope_list)) > 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}



#' Validate that a data.frame doesn't already contained standard column names (internal)
#'
#' Currently used when instantiating one of the three "primal" objects. It will give an
#' error if the user tries to pass a non-standard column name, but an existing column name already
#' uses the standard name.
#'
#' @param data A data.frame
#' @param name The column name selected by the user
#' @param type The type of data (source, sample, or feature)
#'
#' @keywords internal

validate_standard_names = function(data, name, type) {

  if (type == "source") {
    standard = "source_mat_id"
  } else if (type == "sample") {
    standard = "sample_id"
  } else if (type == "feature") {
    standard = "feature_id"
  } else {
    stop(glue::glue("The 'type' argument must be one of 'source', 'sample', or 'feature'. You passed <{type}>."), call. = FALSE)
  }

  if (!name == standard & standard %in% colnames(data)) {
    stop(glue::glue("You are trying to pass the <{name}> column as the '{standard}',
                    but a '{standard}' column already exists in your {type} dataframe.

                    If you really want to use the <{name}> column, it is recommended to
                    rename the existing '{standard}' column, or completely remove it from
                    your dataframe first to avoid collision issues.

                    Sorry for the inconvenience."), call. = FALSE)
  }
}
