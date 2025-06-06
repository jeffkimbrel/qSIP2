% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qsip_objects.R
\name{qsip_source_data}
\alias{qsip_source_data}
\title{qSIP source data class}
\usage{
qsip_source_data(
  data,
  isotope = "isotope",
  isotopolog = "isotopolog",
  source_mat_id = "source_mat_id",
  timepoint = "NULL",
  total_abundance = "NULL",
  volume = "NULL"
)
}
\arguments{
\item{data}{(\emph{dataframe}) Source metadata}

\item{isotope}{(\emph{string}) Isotope name}

\item{isotopolog}{(\emph{string})  Isotopolog data}

\item{source_mat_id}{(\emph{string}) The unique ID for the biological subject or source}

\item{timepoint}{(\emph{string}) Timepoint data}

\item{total_abundance}{(\emph{string}) Total abundance data}

\item{volume}{(\emph{string}) Volume of the abundance data. Defaults to 1, but can be a ul volume if abundance data is given as a concentration}
}
\value{
A validated \code{qsip_source_data} object
}
\description{
The \code{qsip_source_data} object holds validated source material metadata.
}
\details{
\code{qsip_source_data()} is not a typical function, but rather a class constructor that
instantiates a new \code{qsip_source_data} object. The constructor takes a \code{data.frame} as
input and returns a validated \code{qsip_source_data} object.

In qSIP and MISIP, "source material" is your original biological specimen that DNA
was extracted from. This could be a soil sample, a plant, a mouse, etc. This is the
pre-fractionated metadata, and post-fractionation metadata goes in the \code{qsip_sample_data}
object.

Several validation checks are run on the input data:
\itemize{
\item The \code{data} argument must be a \code{data.frame}, including a tibble
\item The \code{isotope}, \code{isotopolog}, and \code{source_mat_id} arguments must be column names in
the \code{data.frame}
\item The \code{source_mat_id} column must be unique
\item The \code{isotope} column must contain valid isotope names. "Valid" means they must be
one of the types that the \code{qSIP2} package has equations for, namely 12C/13C, 14N/15N and 16O/18O.
Some non-isotope names are also valid, including "bulk", "unfractionated" and "T0".
}

Internally, \code{qsip_source_data} renames the metadata columns to be standardized
to MISIP terminology. A \code{data.frame} with the standardized names can be extracted
back out of the object using the \code{get_dataframe()} method, and the optional \code{original_headers}
argument can be set to \code{TRUE} to return the original column names.

One column of metadata that is required although not used by \code{qSIP2} is the
\code{isotopolog} column. This column is required to capture complete metadata that
is compliant with the MISIP standards. However, when running experiments with multiple
isotopologs this column can be used to generate correct comparison groups using
the \code{get_comparison_groups()} function.
}
\seealso{
Other "qSIP Objects": 
\code{\link{qsip_data}()},
\code{\link{qsip_feature_data}()},
\code{\link{qsip_sample_data}()}
}
\concept{"qSIP Objects"}
