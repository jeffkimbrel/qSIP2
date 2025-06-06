% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct.R
\name{iq_get_wad_reference}
\alias{iq_get_wad_reference}
\title{Get unlabeled features for WAD reference (internal)}
\usage{
iq_get_wad_reference(
  qsip_data_object,
  fraction_cutoff = 5,
  source_cutoff = 3,
  quiet = F
)
}
\arguments{
\item{qsip_data_object}{(\emph{qsip_data}) A qsip data object}

\item{fraction_cutoff}{(\emph{numeric, default: 5}) The minimum number of fractions a feature must be found in to be considered}

\item{source_cutoff}{(\emph{numeric, default: 3}) The minimum number of sources a feature must be found in to be considered}

\item{quiet}{(\emph{logical, default: FALSE}) Whether to print a message about the number of features found}
}
\value{
A tibble
}
\description{
Internal function used by the "correct_gpd_" functions to grab unlabeled features
that fit a certain prevalence criteria, and return their mean and median WAD
values, as well as their standard deviation, coefficient of variation, and number
of sources/fractions found in.
}
\keyword{internal}
