#' Remove isotopolog_label from data
#'
#' To convert from data with both isotope and isotopolog_label to one with isotope only
#'
#' @param data A dataframe or tibble
#'
#' @export
#'
#' @keywords sample_data
#'
#' @examples
#' \dontrun{
#' add_isotopolog_label(sample_data_nonMISIP, isotope = "isotope")
#' remove_isotopolog_label(sample_data_MISIP)
#' }

remove_isotopolog_label = function(data) {

  # verify isotope and isotopolog_label columns are found in data
  if (!"isotope" %in% colnames(data)) {
    stop("This dataframe doesn't appear to have isotope data")
  } else if (!"isotopolog_label" %in% colnames(data)) {
    stop("This dataframe doesn't appear to have isotopolog_label data")
  } else {
    data |>
      dplyr::mutate(isotope = dplyr::case_when(
        isotopolog_label == "natural abundance" & isotope == "13C" ~ "12C",
        isotopolog_label == "natural abundance" & isotope == "15N" ~ "14N",
        isotopolog_label == "natural abundance" & isotope == "18O" ~ "16O",
        .default = isotope)
      ) |>
      dplyr::select(-isotopolog_label)
  }
}

utils::globalVariables(c("isotopolog_label", "isotope", "y"))
