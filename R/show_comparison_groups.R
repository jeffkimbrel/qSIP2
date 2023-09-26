#' Show comparison groups
#'
#' Generates a table of ids grouped in columns by isotope, and in rows by the
#' given treatments.
#'
#' @param data Sample metadata (dataframe)
#' @param tmt Treatment value or values (string(s))
#' @param isotope Column name with isotope data (string, default: "isotope")
#' @param source_mat_id Column name with source_mat_id (string, default: "source_mat_id")
#'
#' @export
#'
#' @keywords sample_data
#'
#' @return A dataframe with id grouped by different `tmt` treatments and isotopes

show_comparison_groups = function(data,
                                  tmt,
                                  isotope = "isotope",
                                  source_mat_id = "source_mat_id") {
  data |>
    dplyr::select(!!as.name(source_mat_id), isotope, dplyr::all_of(tmt)) |>
    dplyr::rename(SAMPLES = !!as.name(source_mat_id)) |>
    unique() |>
    tidyr::pivot_wider(names_from = isotope,
                values_from = SAMPLES,
                values_fn = list(SAMPLES = ~paste(., collapse = ", ")))
}
