% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct.R
\name{correct_gpd_bootstrap}
\alias{correct_gpd_bootstrap}
\title{Correct gradient position density (gpd) values using bootstrapped features}
\usage{
correct_gpd_bootstrap(
  qsip_data_object,
  bootstraps = 1000,
  fraction_cutoff = 5,
  source_cutoff = 3,
  quiet = F,
  return = "qsip_data_object"
)
}
\arguments{
\item{qsip_data_object}{(\emph{qsip_data}) A qsip data object}

\item{bootstraps}{(\emph{numeric, default: 1000}) The number of bootstraps to perform}

\item{fraction_cutoff}{(\emph{numeric, default: 5}) The minimum number of fractions a feature must be found in to be considered}

\item{source_cutoff}{(\emph{numeric, default: 3}) The minimum number of sources a feature must be found in to be considered}

\item{quiet}{(\emph{logical, default: FALSE}) Whether to print a message about the number of features found}

\item{return}{(*character, default: "qsip_data_object") Whether to return the corrections or the qsip_data_object}
}
\description{
Correct gradient position density (gpd) values using bootstrapped features
}
