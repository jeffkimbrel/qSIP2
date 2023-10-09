#' Show comparison groups
#'
#' Generates a table of ids grouped in columns by isotope, and in rows by the
#' given treatments.
#'
#' @param source_data (*dataframe, qsip_source_data or qsip_data*) Sample metadata
#' @param tmt (*string*) Treatment value or values
#' @param isotope (*string, default: isotope*) Column name with isotope data
#' @param source_mat_id (*string, default: source_mat_id*) Column name with source_mat_id
#'
#' @export
#'
#' @return A dataframe with id grouped by different `tmt` treatments and isotopes

show_comparison_groups = function(source_data,
                                  tmt,
                                  isotope = "isotope",
                                  source_mat_id = "source_mat_id") {

  if ("qsip_data" %in% class(source_data)) {
    df = source_data@source_data@data
  } else if ("qsip_source_data" %in% class(source_data)) {
    df = source_data@data
  } else if ("data.frame" %in% class(source_data)) {
    df = source_data
  } else {
    class(source_data)
    stop(glue::glue("ERROR: source_data is an unexpected type ({class(source_data)[1]})... it must be class data.frame, qsip_source_data or qsip_data"))
  }


  df |>
    dplyr::select(!!as.name(source_mat_id), !!as.name(isotope), dplyr::all_of(tmt)) |>
    dplyr::rename(SAMPLES = !!as.name(source_mat_id)) |>
    unique() |>
    tidyr::pivot_wider(names_from = !!as.name(isotope),
                values_from = SAMPLES,
                values_fn = list(SAMPLES = ~paste(., collapse = ", ")))
}
