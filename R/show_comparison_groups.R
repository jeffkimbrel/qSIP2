#' Show comparison groups
#'
#' Generates a table of ids grouped in columns by isotope, and in rows by the
#' given treatments.
#'
#' @param source_data (*dataframe, qsip_source_data or qsip_data*) Sample metadata
#' @param group (*string*) Treatment value or values
#' @param isotope (*string, default: isotope*) Column name with isotope data
#' @param source_mat_id (*string, default: source_mat_id*) Column name with source_mat_id
#'
#' @export
#'
#' @return A dataframe with id grouped by different `group` treatments and isotopes
#'


show_comparison_groups <- function(source_data = NULL,
                                   group = NULL,
                                   isotope = "isotope",
                                   source_mat_id = "source_mat_id") {

  if (is.null(source_data)) {
    stop("ERROR: Please provide source data with the 'source_data' argument.")
  }

  if (is.null(group)) {
    stop("ERROR: Please provide a grouping variable with the 'group' argument")
  }

  if ("qsip_data" %in% class(source_data)) {
    df <- source_data@source_data@data
  } else if ("qsip_source_data" %in% class(source_data)) {
    df <- source_data@data
  } else if ("data.frame" %in% class(source_data)) {
    df <- source_data
  } else {
    class(source_data)
    stop(glue::glue("ERROR: source_data is an unexpected type ({class(source_data)[1]})... it must be class data.frame, qsip_source_data or qsip_data"))
  }

  stopifnot("ERROR: Please provide the column name with the source_mat_id" = source_mat_id %in% colnames(df))
  stopifnot("ERROR: Please provide the column name with isotope data" = isotope %in% colnames(df))

  for (g in group) {
    if (!g %in% colnames(df)) {
      stop(glue::glue("ERROR: grouping column '{g}' not found"))
    }
  }

  # bind variables
  SAMPLES <- NULL

  df |>
    dplyr::select(!!as.name(source_mat_id), !!as.name(isotope), dplyr::all_of(group)) |>
    dplyr::rename(SAMPLES = !!as.name(source_mat_id)) |>
    unique() |>
    tidyr::pivot_wider(
      names_from = !!as.name(isotope),
      values_from = SAMPLES,
      values_fn = list(SAMPLES = ~ paste(., collapse = ", "))
    )
}
