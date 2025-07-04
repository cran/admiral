% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_vars_dt_dtm_utils.R
\name{assert_highest_imputation}
\alias{assert_highest_imputation}
\title{Assert Highest Imputation Validity}
\usage{
assert_highest_imputation(
  highest_imputation,
  highest_imputation_values,
  date_imputation = NULL,
  max_dates,
  min_dates
)
}
\arguments{
\item{highest_imputation}{A character scalar indicating the highest level of imputation.

\describe{
\item{Default value}{none}
}}

\item{highest_imputation_values}{A character vector of valid values for \code{highest_imputation}.

\describe{
\item{Default value}{none}
}}

\item{date_imputation}{Optional character scalar specifying the imputation method for dates.

\describe{
\item{Default value}{\code{NULL}}
}}

\item{max_dates}{Optional vector specifying maximum dates for imputation.

\describe{
\item{Default value}{none}
}}

\item{min_dates}{Optional vector specifying minimum dates for imputation.

\describe{
\item{Default value}{none}
}}
}
\value{
Returns \code{NULL} invisibly if assertions pass.
}
\description{
This function checks the validity and requirements for the \code{highest_imputation} argument.
It ensures that necessary conditions for \code{date_imputation}, \code{min_dates},
and \code{max_dates} are met when \code{highest_imputation} is set to \code{"Y"}.
}
\details{
\itemize{
\item If \code{highest_imputation} is "Y", either \code{min_dates} or \code{max_dates} must be specified.
\item If \code{highest_imputation} is "Y" and \code{date_imputation} is "first",
\code{min_dates} must be specified.
\item If \code{highest_imputation} is "Y" and \code{date_imputation} is "last",
\code{max_dates} must be specified.
}
}
\keyword{internal}
