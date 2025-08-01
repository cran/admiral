% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_query_data.R
\name{assert_terms}
\alias{assert_terms}
\title{Asserts Requirements for Terms for Queries}
\usage{
assert_terms(terms, expect_grpname = FALSE, expect_grpid = FALSE, source_text)
}
\arguments{
\item{terms}{Terms provided by user

\describe{
\item{Default value}{none}
}}

\item{expect_grpname}{Is the \code{GRPNAME} column expected?

\describe{
\item{Default value}{\code{FALSE}}
}}

\item{expect_grpid}{Is the \code{GRPID} column expected?

\describe{
\item{Default value}{\code{FALSE}}
}}

\item{source_text}{Text describing the source of the terms, e.g., \verb{"the data frame provided for the }definition\verb{ element"}.

\describe{
\item{Default value}{none}
}}
}
\value{
An error is issued if
\itemize{
\item \code{terms} is not a data frame,
\item \code{terms} has zero observations,
\item the \code{SRCVAR} variable is not in \code{terms},
\item neither the \code{TERMCHAR} nor the \code{TERMNUM} variable is in \code{terms},
\item \code{expect_grpname == TRUE} and the \code{GRPNAME} variable is not in \code{terms},
\item \code{expect_grpid == TRUE} and the \code{GRPID} variable is not in \code{terms},
}
}
\description{
The function checks the requirements for terms for queries provided by the
user. The terms could have been provided directly in the query definition or
via a user provided function for accessing a SMQ or SDG database.
}
\examples{

try(
  assert_terms(
    terms = 42,
    source_text = "object provided by the `definition` element"
  )
)

}
\keyword{internal}
