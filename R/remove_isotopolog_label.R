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
  # verify isotope and isotopolog_label columns are found in data

  stopifnot("ERROR: This dataframe doesn't appear to have isotope data" = "isotope" %in% colnames(data))
  stopifnot("ERROR: This dataframe doesn't appear to have isotopolog_label data" = "isotopolog_label" %in% colnames(data))

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
