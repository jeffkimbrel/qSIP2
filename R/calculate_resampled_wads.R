#' Internal function for resampling WAD values (internal)
#'
#' Designed as a purrr::map() function called by run_resampling.R
#'
#' @param i (*integer*) The specific iteration of the resampling
#' @param wad_data (*dataframe*) A WAD dataframe to resample columns from
#' @param type (*string*) Text for whether the wad data is from labeled or unlabeled data
#'
#' @returns The resampling data that will be boot in `@resamples`
#'
#' @export

calculate_resampled_wads <- function(i, wad_data, type, allow_failures = FALSE) {

  # make sure all data is numeric or NA
  stopifnot("wad dataframe to resample from contains non-numeric data" = all(sapply(wad_data, is.numeric)))

  # make a new names vector to abstract away the real names into a numbered list of names
  new_names <- c("feature_id", paste(type, seq(1:(ncol(wad_data))), sep = "_"))
  wad_data_resampled <- wad_data[, sample(ncol(wad_data), replace = T, size = ncol(wad_data)), drop = FALSE]

  if (allow_failures == FALSE) {
    # double check the dimensions remain the same after removing all rows with NA.
    wad_data_resampled_noNA = wad_data_resampled[rowSums(is.na(wad_data_resampled)) != ncol(wad_data_resampled), ]
    if (class(wad_data_resampled_noNA) == "numeric") {
      wad_data_resampled_noNA_length = length(wad_data_resampled_noNA)
    } else if (class(wad_data_resampled_noNA) == "data.frame") {
      wad_data_resampled_noNA_length = nrow(wad_data_resampled_noNA)
    }

    if (identical(nrow(wad_data), wad_data_resampled_noNA_length) == FALSE) {
      stop(("Something went wrong with resampling...\nIt is possible that some resampled features contained only <NA> WAD values leading to a failure in calculate_Z().\nTry increasing your filtering stringency to remove features not found in most sources"), call. = FALSE)
    }
  } else if (allow_failures == TRUE) {
    # just remove rows that are all NAs... this will reduce the number of successful resamples reported for this feature/type

    if (any(is.na(wad_data_resampled))) {
      wad_data_resampled = na.omit(wad_data_resampled)
    }

  }

  # save the original names, in case they are needed later
  wad_data_resampled_names <- colnames(wad_data_resampled)

  # bring the feature id from the rownames back to a column, repair the names,
  # add the resample # and the type, and arrange just for fun
  wad_data_resampled <- wad_data_resampled |>
    tibble::rownames_to_column("feature_id") |>
    tibble::as_tibble(.name_repair = ~new_names) |>
    dplyr::mutate(resample = i, .after = feature_id) |>
    dplyr::mutate(type = type, .after = "feature_id") |>
    dplyr::arrange(feature_id)

  wad_data_resampled
}
