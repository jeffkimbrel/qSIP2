% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validate_gradient_pos_density}
\alias{validate_gradient_pos_density}
\title{Check the validity of density values (internal)}
\usage{
validate_gradient_pos_density(df, low = 1.55, high = 1.8)
}
\arguments{
\item{df}{(\emph{dataframe}) A two-column dataframe with density positions and density values}

\item{low}{(\emph{numeric, default: 1.55}) A low limit for valid density values}

\item{high}{(\emph{numeric, default: 1.8}) A high limit for valid density values}
}
\value{
Returns \code{NULL} if the density values are valid, or a printed error
}
\description{
Check the validity of density values (internal)
}
\keyword{internal}
