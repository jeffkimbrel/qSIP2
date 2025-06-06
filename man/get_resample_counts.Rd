% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_resample_counts}
\alias{get_resample_counts}
\title{Get counts of successful resampling}
\usage{
get_resample_counts(qsip_data_object, as_percentage = FALSE)
}
\arguments{
\item{qsip_data_object}{(\emph{qsip_data}) A qsip data object that has been resampled}

\item{as_percentage}{(\emph{logical}) If TRUE, the counts will be returned as a percentage of the total number of resamples}
}
\value{
A dataframe with columns feature_id, type, and n
}
\description{
For each feature_id and label type, this function will return the counts of successful
resampling. This value will typically be the number of resamples given to \code{run_resampling()},
but if \code{run_resampling()} is called with \code{allow_failures = TRUE} then the number of
successful resamples might be less than the number of resamples given.
}
\details{
If as_percentage is TRUE, the counts will be returned as a percentage of
the total number of resamples.
}
