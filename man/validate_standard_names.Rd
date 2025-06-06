% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validate_standard_names}
\alias{validate_standard_names}
\title{Validate that a data.frame doesn't already contained standard column names (internal)}
\usage{
validate_standard_names(data, name, type)
}
\arguments{
\item{data}{A data.frame}

\item{name}{The column name selected by the user}

\item{type}{The type of data (source, sample, or feature)}
}
\description{
Currently used when instantiating one of the three "primal" objects. It will give an
error if the user tries to pass a non-standard column name, but an existing column name already
uses the standard name.
}
\keyword{internal}
