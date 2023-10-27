#' Internal function for resampling WAD values
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

calculate_resampled_wads <- function(i, wad_data, type) {

  # make sure all data is numeric or NA
  stopifnot("ERROR: wad dataframe to resample from contains non-numeric data" = all(sapply(wad_data, is.numeric)))

  # make a new names vector to abstract away the real names into a numbered list of names
  new_names <- c("feature_id", paste(type, seq(1:(ncol(wad_data))), sep = "_"))

  wad_data_resampled <- wad_data[, sample(ncol(wad_data), replace = T, size = ncol(wad_data)), drop = FALSE]

  # double check the dimensions remain the same. Not covered by testthat
  if (identical(dim(wad_data), dim(wad_data_resampled)) == FALSE) {
    stop("ERROR: something went wrong with resampling...")
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
