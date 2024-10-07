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
