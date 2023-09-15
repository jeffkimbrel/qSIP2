#' Add isotopolog_label to data
#'
#' To convert from an isotope only dataframe to one with both isotope and isotopolog_label to satisfy MISIP requirements
#'
#' Note, this function also renames your isotope column to "isotope", if it isn't already.
#'
#' @param data A dataframe or tibble
#' @param isotope Column name isotope data
#'
#' @export
#'
#' @keywords sample_data

add_isotopolog_label = function(data, isotope) {

  # verify isotope column is found in data
  if (!isotope %in% colnames(data)) {
    stop("Please provide a valid column name for the isotope data")
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
