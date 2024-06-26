% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MISIP.R
\name{remove_isotopolog_label}
\alias{remove_isotopolog_label}
\title{Remove isotopolog_label from "MISIPified" data}
\usage{
remove_isotopolog_label(data)
}
\arguments{
\item{data}{(\emph{dataframe}) Sample metadata}
}
\value{
A dataframe with \code{isotopolog_label} column removed and \code{isotope}
column modified
}
\description{
To convert from data with both isotope and isotopolog_label to one with
isotope only.
}
\seealso{
Other "MISIP": 
\code{\link{add_isotopolog_label}()}
}
\concept{"MISIP"}
