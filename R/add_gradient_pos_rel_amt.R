#' Add gradient_pos_rel_amt to data
#'
#' This function will calculate the relative amount of a fraction compared to
#' the whole replicate using either qPCR copies or DNA concentrations.
#'
#' @param data (*dataframe*) Sample metadata
#' @param amt (*string*) Column name that has the qPCR or DNA amounts per fraction
#' @param source_mat_id (*string, default: "source_mat_id"*) Grouping variable for a replicate
#' @param overwrite (*bool, default: FALSE*) Determines whether or not to overwrite an existing gradient_pos_rel_amt column
#'
#' @export
#'
#' @family "Sample Data"
#'
#' @returns A dataframe with a `gradient_pos_rel_amt` column

add_gradient_pos_rel_amt <- function(data,
                                     amt,
                                     source_mat_id = "source_mat_id",
                                     overwrite = FALSE) {

  # make sure column names exist
  if (!amt %in% colnames(data)) {
    stop(glue::glue("{amt} not found in dataframe"), call. = FALSE)
  }
  if (!source_mat_id %in% colnames(data)) {
    stop(glue::glue("{source_mat_id} not found in dataframe"), call. = FALSE)
  }


  if ("gradient_pos_rel_amt" %in% colnames(data)) {
    if (overwrite == FALSE) {
      stop("gradient_pos_rel_amt already exists! Set overwrite = TRUE if you want to overwrite", call. = FALSE)
    } else if (overwrite == TRUE) {
      message("WARNING: gradient_pos_rel_amt already exists and will be overwritten")
    }
  }

  data |>
    dplyr::mutate(
      gradient_pos_rel_amt = !!as.name(amt) / sum(!!as.name(amt)),
      .by = !!as.name(source_mat_id)
    )
}
