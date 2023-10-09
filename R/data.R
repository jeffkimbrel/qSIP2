#' Example qSIP Sample Data (MISIP)
#'
#' An example sample data tibble that (mostly) matches the MISIP standards.
#'
#' @format ## `sample_data_MISIP`
#' A data frame with 72 rows and 7 columns:
#' \describe{
#'   \item{source_mat_id}{The unique name of the biological replicate}
#'   \item{isotope}{The labeled isotope}
#'   \item{isotopolog_label}{Whether the isotopolog has the labeled or unlabeled substrate}
#'   \item{gradient_position}{The gradient position ranked from highest to lowest density}
#'   \item{gradient_pos_density}{The density of the fraction}
#'   \item{gradient_pos_dna}{The DNA concentration of the fraction}
#'   \item{gradient_pos_rel_amt}{The relative amount in that fraction from either qPCR or DNA concentrations}
#' }
#'
#' @keywords data
"sample_data_MISIP"



#' Example qSIP Sample Data (non MISIP)
#'
#' An example sample data tibble that is not formatted according to MISIP standards.
#'
#' @format ## `sample_data_nonMISIP`
#' A data frame with 72 rows and 5 columns:
#' \describe{
#'   \item{rep}{The unique name of the biological replicate}
#'   \item{isotope}{The labeled isotope}
#'   \item{fraction}{The gradient position ranked from highest to lowest density}
#'   \item{density}{The density of the fraction}
#'   \item{dna_conc}{The DNA concentration of the fraction}
#' }
#'
#' @keywords data
"sample_data_nonMISIP"
