% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_vars_dt_dtm_utils.R
\name{assert_time_imputation}
\alias{assert_time_imputation}
\title{Assert \code{time_imputation}}
\usage{
assert_time_imputation(time_imputation, highest_imputation)
}
\arguments{
\item{time_imputation}{The value to impute time when missing

\describe{
\item{Default value}{none}
}}

\item{highest_imputation}{Highest imputation level

\describe{
\item{Default value}{none}
}}
}
\value{
asserted \code{time_imputation}
}
\description{
Applies assertions on the \code{time_imputation} argument
}
\examples{
# Assert valid 'first' time imputation
time_imp_first <- admiral:::assert_time_imputation("first", "Y")
print(time_imp_first)

# Assert valid 'last' time imputation
time_imp_last <- admiral:::assert_time_imputation("last", "Y")
print(time_imp_last)

# Assert valid custom time imputation "12:34:56"
time_imp_custom <- admiral:::assert_time_imputation("12:34:56", "Y")
print(time_imp_custom)

}
\keyword{internal}
