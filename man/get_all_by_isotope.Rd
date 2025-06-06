% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_all_by_isotope}
\alias{get_all_by_isotope}
\title{Get source_mat_ids meeting certain isotope conditions}
\usage{
get_all_by_isotope(
  qsip_data_object,
  isotopes,
  quiet = FALSE,
  silent = lifecycle::deprecated()
)
}
\arguments{
\item{qsip_data_object}{(\emph{qsip_data or qsip_source_data}) A qsip object with source data}

\item{isotopes}{(\emph{string(s)}) Isotopes used to pull source_mat_ids. Can be a standard isotope name (e.g. \verb{12C}) or special terms \code{labeled} or \code{unlabeled}}

\item{quiet}{(\emph{boolean}) If \code{TRUE}, suppresses messages about missing isotope hits and doesn't fail}

\item{silent}{Deprecated, use \code{quiet} instead}
}
\value{
A vector of source_mat_ids. It may also print some messages.
}
\description{
Get source_mat_ids meeting certain isotope conditions
}
