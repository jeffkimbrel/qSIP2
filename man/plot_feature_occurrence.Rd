% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_feature_occurrence}
\alias{plot_feature_occurrence}
\title{Plot occurrence of features in samples}
\usage{
plot_feature_occurrence(
  qsip_data_object,
  feature_ids = NULL,
  scale = "none",
  show_wad = FALSE,
  title = NULL,
  legend.position = "right"
)
}
\arguments{
\item{qsip_data_object}{(\emph{qsip_data}) A qsip_data object}

\item{feature_ids}{(\emph{character}) An optional vector of feature_ids}

\item{scale}{(\emph{character}) A character string}

\item{show_wad}{(\emph{logical}) A logical value}

\item{title}{(\emph{character}) A character string}

\item{legend.position}{(\emph{character} or \emph{numeric vector}) Values passed to ggplot2::theme(legend.position = ...)}
}
\value{
Returns a ggplot object
}
\description{
This is a plotting function to visualize the occurrence of features in samples.
The function takes a qsip_data object and a vector of feature_ids, and can scale
the results by total abundance or source abundance, and the WAD value can also be shown.
}
