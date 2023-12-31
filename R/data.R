#' Example Source Dataframe
#'
#' An example source data table with top-level (pre-fractionation) metadata
#'
#' @format ## `example_source_df`
#' A data frame with 15 rows and 5 columns:
#' \describe{
#'   \item{source}{The unique name of the biological replicate or source_mat_id}
#'   \item{total_copies_per_g}{The total amplicon copies as determined by qPCR}
#'   \item{total_dna}{The total DNA isolated from the source}
#'   \item{Isotope}{The isotope used in this source}
#'   \item{Moisture}{An example of a relevant treatment}
#' }
#'
#' @family "example datasets"
"example_source_df"


#' Example Sample Dataframe
#'
#' An example sample data table with fraction-related metadata
#'
#' @format ## `example_sample_df`
#' A data frame with 284 rows and 6 columns:
#' \describe{
#'   \item{sample}{The unique sample id of the fraction}
#'   \item{source}{The parent source id for this sample}
#'   \item{Fraction}{The gradient position}
#'   \item{density_g_ml}{The density of this fraction}
#'   \item{dna_conc}{The concentration of DNA post-fractionation}
#'   \item{avg_16S_g_soil}{qPCR amplicon count}
#' }
#'
#' @family "example datasets"
"example_sample_df"


#' Example Feature Abundance Dataframe
#'
#' An example feature table with abundances
#'
#' @format ## `example_feature_df`
#' A data frame with 2,030 rows and 285 columns:
#' \describe{
#'   \item{ASV}{The unique feature ID}
#'   \item{Columns 2-285}{IDs for all samples}
#' }
#'
#' @family "example datasets"
"example_feature_df"



#' Example qSIP Source Object
#'
#' An example qsip_source_data object
#'
#' @format ## `example_source_object`
#'
#' @family "example datasets"
"example_source_object"

#' Example qSIP Sample Object
#'
#' An example qsip_sample_data object
#'
#' @format ## `example_sample_object`
#'
#' @family "example datasets"
"example_sample_object"

#' Example qSIP Feature Object
#'
#' An example qsip_feature_data object
#'
#' @format ## `example_feature_object`
#'
#' @family "example datasets"
"example_feature_object"


#' Example qSIP Object
#'
#' An example qsip_data object
#'
#' @format ## `example_qsip_object`
#'
#' @family "example datasets"
"example_qsip_object"
