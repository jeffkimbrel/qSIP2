% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qsip_objects.R
\name{qsip_sample_data}
\alias{qsip_sample_data}
\title{qSIP sample data class}
\usage{
qsip_sample_data(
  data,
  sample_id = "sample_id",
  source_mat_id = "source_mat_id",
  gradient_position = "gradient_position",
  gradient_pos_density = "gradient_pos_density",
  gradient_pos_amt = "gradient_pos_amt",
  gradient_pos_rel_amt = "",
  convert_negatives = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{data}{(\emph{dataframe}) Metadata for samples/fractions}

\item{sample_id}{(\emph{string}) The unique sample ID}

\item{source_mat_id}{(\emph{string}) The unique ID for the biological subject or replicate}

\item{gradient_position}{(\emph{string}) Column name with the fraction position}

\item{gradient_pos_density}{(\emph{string}) Column name with the gradient density}

\item{gradient_pos_amt}{(\emph{string}) Column name with a total amount per fraction, either
qPCR copies or DNA}

\item{gradient_pos_rel_amt}{(\emph{string}) Column name with the relative fraction abundance
compared to the total}

\item{convert_negatives}{(\emph{boolean, default: FALSE}) Whether or not to convert negative amounts to zero}

\item{overwrite}{(\emph{boolean, default: FALSE}) Whether or not to overwrite the gradient_pos_rel_amt values}
}
\value{
A validated object of the \code{qsip_sample_data} type
}
\description{
The \code{qsip_sample_data} object holds validated sample metadata.
}
\details{
\code{qsip_sample_data()} is not a typical function, but rather a class constructor that
instantiates a new \code{qsip_sample_data} object. The constructor takes a \code{data.frame} as
input and returns a validated \code{qsip_sample_data} object.

In qSIP and MISIP, a "sample" is the post-fractionated material with metadata
pertaining to the fractionation process. Sample metadata contains information
about the sample and fractionation, such as the sample ID, the source material
ID, the gradient position, the density, the amount recovered (e.g. DNA concentration
or 16S copies), and the relative abundance of the fraction compared to the total.

Ideally, \code{gradient_pos_amt} should be reported as a mass value of DNA rather than
a concentration. However, if the concentration is reported, the \code{fraction_volume}
argument can be used to convert the \code{gradient_pos_amt} concentration to a mass value.
For example, if the \code{gradient_pos_amt} is reported as ng/ul, and the \code{fraction_volume}
is reported as 100 ul, then the \code{gradient_pos_amt} will be converted to ng.

Internally, \code{qsip_sample_data} renames the metadata columns to be standardized
to MISIP terminology. A \code{data.frame} with the standardized names can be extracted
back out of the object using the \code{get_dataframe()} method, and the optional \code{original_headers}
argument can be set to \code{TRUE} to return the original column names.

There are several validation checks done on the \code{data.frame}:
\itemize{
\item The \code{data} argument must contain a \code{data.frame}, including a tibble
\item The \code{sample_id} column must contain unique values per row
\item The \code{gradient_position} must container positive integers, or \code{-1} is allowed to
designate the sample as "bulk" or unfractionated
}
}
\seealso{
Other "qSIP Objects": 
\code{\link{qsip_data}()},
\code{\link{qsip_feature_data}()},
\code{\link{qsip_source_data}()}
}
\concept{"qSIP Objects"}
