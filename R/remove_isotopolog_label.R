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
#' @export
#'
#' @family MISIP

remove_isotopolog_label_check = function(df, isotope = "isotope") {
  if (all(df[[isotope]] %in% c("13C", "15N", "18O"))) {
    df |>
      qSIP2::remove_isotopolog_label()
  }
}

