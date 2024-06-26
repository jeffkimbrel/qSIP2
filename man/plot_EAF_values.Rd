% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_EAF_values.R
\name{plot_EAF_values}
\alias{plot_EAF_values}
\title{Plot EAF and confidence intervals}
\usage{
plot_EAF_values(
  qsip_data_object,
  confidence = 0.9,
  success_ratio = 0.9,
  top = Inf,
  error = "none",
  alpha = 0.3,
  zero_line = TRUE
)
}
\arguments{
\item{qsip_data_object}{(\emph{qsip_data}) A qsip_data object or list of qsip_data objects}

\item{confidence}{(\emph{numeric}) The confidence level for the confidence interval}

\item{success_ratio}{(\emph{numeric}) The ratio of successful resamples to total resamples}

\item{top}{(\emph{numeric}) The number of top features to plot. Use \code{Inf} for all}

\item{error}{(\emph{character}) The type of error bars to plot. Options are 'none', 'bar', 'ribbon'}

\item{alpha}{(\emph{numeric}) The transparency of the error bar/ribbon}

\item{zero_line}{(\emph{logical}) Add a line at EAF = 0}
}
\description{
This function plots the observed EAF values for each feature in the dataset.
The features are ordered by their observed EAF values. The confidence intervals
are plotted as error bars or ribbons. The points are colored based on the
success ratio of the resamples.
}
