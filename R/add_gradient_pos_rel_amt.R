#' Add gradient_pos_rel_amt to data
#'
#' This function will calculate the relative amount of a fraction compared to
#' the whole replicate using either qPCR copies or DNA concentrations.
#'
#' @param data Sample metadata (dataframe)
#' @param amt Column name that has the qPCR or DNA amounts per fraction (string)
#' @param source_mat_id Grouping variable for a replicate (string, default: "source_mat_id")
#' @param overwrite Determines whether or not to overwrite an existing gradient_pos_rel_amt column (bool, default: FALSE)
#'
#' @export
#'
#' @keywords sample_data
#'
#' @return A dataframe with a `gradient_pos_rel_amt` column


add_gradient_pos_rel_amt = function(data,
                                    amt,
                                    source_mat_id = "source_mat_id",
                                    overwrite = FALSE) {

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
