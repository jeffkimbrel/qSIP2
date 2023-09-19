#' Add gradient_pos_rel_amt to data
#'
#' This function will calculate the relative amt of a fraction compared to the
#' whole replicate using either qPCR copies or DNA concentrations.
#'
#' @param data A dataframe or tibble
#' @param source_mat_id Grouping variable for a replicate
#' @param amt Column name that has the qPCR or DNA amounts per fraction
#' @param overwrite Determines whether or not to overwrite an existing gradient_pos_rel_amt column
#'
#' @export
#'
#' @keywords sample_data

add_gradient_pos_rel_amt = function(data,
                                    source_mat_id = "source_mat_id",
                                    amt,
                                    overwrite = F) {

  if ("gradient_pos_rel_amt" %in% colnames(data)) {
    if (overwrite == FALSE) {
      stop(crayon::red("gradient_pos_rel_amt already exists! Set overwrite = TRUE if you want to overwrite"))
    } else if (overwrite == TRUE) {
      message("gradient_pos_rel_amt already exists and will be overwritten")
    }
  }

  data |>
    dplyr::mutate(gradient_pos_rel_amt = !!as.name(amt) / sum(!!as.name(amt)),
                  .by = !!as.name(source_mat_id))
}
