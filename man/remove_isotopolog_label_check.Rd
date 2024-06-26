% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MISIP.R
\name{remove_isotopolog_label_check}
\alias{remove_isotopolog_label_check}
\title{Remove isotopolog label if needed}
\usage{
remove_isotopolog_label_check(df, isotope = "isotope")
}
\arguments{
\item{df}{Sample data}

\item{isotope}{Column name with isotope data}
}
\description{
The qSIP2 functions expect non-MISIP data. If running qSIP2 within KBase, then this function
will verify/update the data to be non-MISIPified.
}
\concept{MISIP}
