% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_feature_curves}
\alias{plot_feature_curves}
\title{Plot qSIP feature data density curves}
\usage{
plot_feature_curves(
  qsip_data_object,
  feature_ids,
  source_mat_ids = NULL,
  scale = "source",
  color_by = "source",
  title = NULL
)
}
\arguments{
\item{qsip_data_object}{(\emph{qsip_data}) A qsip data object with tube relative abundances}

\item{feature_ids}{(\emph{string}) Feature ids to be plotted on their own facet}

\item{source_mat_ids}{(\emph{string, defaults to all}) A list of source material ids}

\item{scale}{(\emph{string}) A string indicating how to scale the y-axis. Options are "feature" or "source"}

\item{color_by}{(\emph{string}) A string indicating how to color the lines. Options are "source" or "isotope"}

\item{title}{(\emph{string}) An optional title for the plot}
}
\value{
A ggplot object
}
\description{
Plot qSIP feature data density curves
}
\seealso{
Other "visualizations": 
\code{\link{plot_density_outliers}()},
\code{\link{plot_filter_results}()},
\code{\link{plot_sample_curves}()},
\code{\link{plot_source_wads}()}
}
\concept{"visualizations"}
