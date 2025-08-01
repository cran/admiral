% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_vars_dt_dtm_utils.R
\name{is_partial_datetime}
\alias{is_partial_datetime}
\title{Check if a Partial Date/Time is a Datetime}
\usage{
is_partial_datetime(partial)
}
\arguments{
\item{partial}{A named list containing date or datetime components.

\describe{
\item{Default value}{none}
}}
}
\value{
A logical value. TRUE if the partial represents a datetime,
FALSE if it represents a date only.
}
\description{
This function determines whether a given partial date/time structure represents
a datetime or just a date.
}
\details{
The function checks for the presence of all date components (year, month, day)
and all time components (hour, minute, second) in the input list. If all components
are present, it's considered a datetime.
If only date components are present, it's considered a date.
Any other combination will result in an error.
}
\examples{
# Datetime example
partial_datetime <- list(
  year = "2023", month = "05", day = "15",
  hour = "14", minute = "30", second = "00"
)
admiral:::is_partial_datetime(partial_datetime) # Returns TRUE

# Date example
partial_date <- list(year = "2023", month = "05", day = "15")
admiral:::is_partial_datetime(partial_date) # Returns FALSE

# Invalid example
\dontrun{
partial_invalid <- list(year = "2023", month = "05", hour = "14")
admiral:::is_partial_datetime(partial_invalid) # Throws an error
}

}
\keyword{internal}
