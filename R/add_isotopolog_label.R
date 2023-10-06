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
#' @family "Source Data"
#'
#' @returns A dataframe with `isotopolog_label` column added and `isotope` column
#' modified

add_isotopolog_label = function(data, isotope = "isotope") {

  # verify isotope column is found in data
  if (!isotope %in% colnames(data)) {
    stop("ERROR: Please provide a valid column name for the isotope data")
  } else {
    data |>
      dplyr::rename("isotope" = isotope) |>
      dplyr::mutate(isotopolog_label = dplyr::case_when(
        isotope %in% c("12C", "14N", "16O") ~ "natural abundance",
        isotope %in% c("13C", "15N", "18O") ~ "isotopically labeled",
        .default = NA
      ),
      .after = "isotope") |>
      dplyr::mutate(isotope = dplyr::case_when(
        isotopolog_label == "natural abundance" & isotope == "12C" ~ "13C",
        isotopolog_label == "natural abundance" & isotope == "14N" ~ "15N",
        isotopolog_label == "natural abundance" & isotope == "16O" ~ "18O",
        .default = isotope)
      )
  }

}
