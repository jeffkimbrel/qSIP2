#' Remove isotopolog_label from "MISIPified" data
#'
#' To convert from data with both isotope and isotopolog_label to one with
#' isotope only.
#'
#' @param data (*dataframe*) Sample metadata
#'
#' @return A dataframe with `isotopolog_label` column removed and `isotope`
#' column modified
#'
#' @export
#'
#' @family "MISIP"

remove_isotopolog_label <- function(data) {

  # verify data is the right type
  stopifnot("data should be class <data.frame>" = "data.frame" %in% class(data))

  # verify isotope and isotopolog_label columns are found in data
  stopifnot("isotope column not found" = "isotope" %in% colnames(data))
  stopifnot("isotopolog_label column not found" = "isotopolog_label" %in% colnames(data))

  # must be a heavy isotope
  qSIP2::validate_isotopes(data$isotope, isotope_list = c("13C", "15N", "18O"))

  # bind variables
  isotope <- isotopolog_label <- NULL

  data |>
    dplyr::mutate(isotope = dplyr::case_when(
      isotopolog_label == "natural abundance" & isotope == "13C" ~ "12C",
      isotopolog_label == "natural abundance" & isotope == "15N" ~ "14N",
      isotopolog_label == "natural abundance" & isotope == "18O" ~ "16O",
      .default = isotope
    )) |>
    dplyr::select(-isotopolog_label)

}



#' Remove isotopolog label if needed
#'
#' The qSIP2 functions expect non-MISIP data. If running qSIP2 within KBase, then this function
#' will verify/update the data to be non-MISIPified.
#'
#' @param df Sample data
#' @param isotope Column name with isotope data
#'
#' @export
#'
#' @family MISIP

remove_isotopolog_label_check = function(df, isotope = "isotope") {
  if (all(df[[isotope]] %in% c("13C", "15N", "18O"))) {
    df |>
      qSIP2::remove_isotopolog_label()
  } else {
    df
  }
}

#' Add isotopolog_label to source data
#'
#' To convert from an isotope only dataframe to one with both isotope and
#' isotopolog_label to satisfy MISIP requirements
#'
#' @param data (*dataframe*) Sample metadata
#' @param isotope (*string, default: "isotope"*) Column name with isotope data
#'
#' @export
#'
#' @family "MISIP"
#'
#' @returns A dataframe with `isotopolog_label` column added and `isotope` column
#' modified

add_isotopolog_label <- function(data, isotope = "isotope") {

  # verify isotope column is found in data
  if (!isotope %in% colnames(data)) {
    stop(glue::glue("{isotope} column not found"), call. = FALSE)
  }

  data |>
    dplyr::rename("isotope" = isotope) |>
    dplyr::mutate(
      isotopolog_label = dplyr::case_when(
        isotope %in% c("12C", "14N", "16O") ~ "natural abundance",
        isotope %in% c("13C", "15N", "18O") ~ "isotopically labeled",
        .default = NA
      ),
      .after = "isotope"
    ) |>
    dplyr::mutate(isotope = dplyr::case_when(
      isotopolog_label == "natural abundance" & isotope == "12C" ~ "13C",
      isotopolog_label == "natural abundance" & isotope == "14N" ~ "15N",
      isotopolog_label == "natural abundance" & isotope == "16O" ~ "18O",
      .default = isotope
    ))
}
