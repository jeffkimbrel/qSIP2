% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{fit_regression_model}
\alias{fit_regression_model}
\title{Fit regression model to spike-in control data}
\usage{
fit_regression_model(control_data, method = "lm")
}
\arguments{
\item{control_data}{A data frame containing the spike-in control data}

\item{method}{The regression engine to use. Choices are "lm" or "glm"}
}
\value{
a workflow object
}
\description{
This is an internal function that does the actual fitting of the regression
model to the spike-in control data, and is called by \code{jgi_normalize_features()}
via a \code{purrr::map()} call after nesting the data.
}
\details{
The output is a workflow object that can be used for fitting with
\code{broom::augment(parsnip::extract_fit_engine(x))}.
}
\seealso{
Other "spike-ins": 
\code{\link{get_normalized_controls}()},
\code{\link{get_normalized_features}()},
\code{\link{jgi_feature_df}()},
\code{\link{jgi_normalize_features}()},
\code{\link{jgi_sample_df}()},
\code{\link{jgi_source_df}()}
}
\concept{"spike-ins"}
\keyword{internal}
