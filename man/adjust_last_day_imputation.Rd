% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_vars_dt_dtm_utils.R
\name{adjust_last_day_imputation}
\alias{adjust_last_day_imputation}
\title{Adjust Last Day Imputation}
\usage{
adjust_last_day_imputation(imputed_dtc, partial)
}
\arguments{
\item{imputed_dtc}{A character vector of imputed date/datetime strings.

\describe{
\item{Default value}{none}
}}

\item{partial}{A list of partial date/time components.

\describe{
\item{Default value}{none}
}}
}
\value{
A character vector of adjusted date/datetime strings.
}
\description{
This functions adjusts the day of the imputed date to the last day the month
if the day was imputed. It should be called if \code{date_imputation = "last"} was used
for the date imputation as \code{get_imputation_target_date()} imputes the last day
as \code{"28"}.
}
\details{
If the day component in \code{partial} is missing,
the day (in \code{imputed_dtc}) is adjusted to the last day of the month.
}
\examples{
# Adjust last day imputation for a date with an incomplete day
imputed_date <- "2021-03-28"
partial_date <- list(year = "2021", month = "03", day = NA_character_)
admiral:::adjust_last_day_imputation(imputed_date, partial_date)

# Adjust last day imputation for a datetime with missing day
imputed_datetime <- "2021-03-28T00:00:00"
partial_datetime <- list(
  year = "2021", month = "03", day = NA_character_,
  hour = "00", minute = "00", second = "00"
)
admiral:::adjust_last_day_imputation(imputed_datetime, partial_datetime)

# Adjust last day imputation for a date with known day
partial_date_known_day <- list(year = "2021", month = "03", day = "15")
adjusted_date_known_day <- admiral:::adjust_last_day_imputation(
  imputed_date,
  partial_date_known_day
)
print(adjusted_date_known_day)

# Adjust last day imputation for a datetime with known day
partial_datetime_known_day <- list(
  year = "2021", month = "03", day = "15",
  hour = "00", minute = "00", second = "00"
)
adjusted_datetime_known_day <- admiral:::adjust_last_day_imputation(
  imputed_datetime,
  partial_datetime_known_day
)
print(adjusted_datetime_known_day)

}
\keyword{internal}
