% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qsip_objects.R
\name{qsip_data}
\alias{qsip_data}
\title{qSIP master data class}
\usage{
qsip_data(source_data, sample_data, feature_data)
}
\arguments{
\item{source_data}{(\emph{qsip_source_data}) A qSIP source data object}

\item{sample_data}{(\emph{qsip_sample_data}) A qSIP sample data object}

\item{feature_data}{(\emph{qsip_feature_data}) A qSIP feature data object}
}
\value{
A validated \code{qsip_data} object
}
\description{
The \code{qsip_data} object holds validated qSIP source, sample and feature metadata,
and has slots to store all of the subsequence \code{qSIP2} analysis.
}
\details{
\code{qsip_data()} is not a typical function, but rather a class constructor that
instantiates a new \code{qsip_data} object. The constructor takes a \code{qsip_source_data},
\code{qsip_sample_data} and \code{qsip_feature_data} as input and returns a validated \code{qsip_data}
object.

This \code{qsip_data} object holds the source, sample and feature data. It also creates empty
slots to hold the filtering results, the resampling and the EAF values from their
associated functions. For this reason, the \code{qsip_data} object is intended to be
progressively overwritten with new analysis results, but new objects can be created
at any point in the analysis, if necessary. For example, a study with multiple comparison
groups might be combined into one large \code{qSIP_data} object, and then split into separate
objects at the \code{run_feature_filtering()} step.

Internally, creating the original qSIP objects renames the metadata columns to be standardized
to MISIP terminology. A \code{data.frame} with the standardized names can be extracted
back out of the \code{qSIP_data} using the \code{get_dataframe()} method and a required \code{type} argument
of "source", "sample" or "feature". The optional \code{original_headers}
argument can be set to \code{TRUE} to return the original column names.
}
\seealso{
Other "qSIP Objects": 
\code{\link{qsip_feature_data}()},
\code{\link{qsip_sample_data}()},
\code{\link{qsip_source_data}()}
}
\concept{"qSIP Objects"}
