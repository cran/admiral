% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_vars_dtm.R
\name{derive_vars_dtm}
\alias{derive_vars_dtm}
\title{Derive/Impute a Datetime from a Character Date}
\usage{
derive_vars_dtm(
  dataset,
  new_vars_prefix,
  dtc,
  highest_imputation = "h",
  date_imputation = "first",
  time_imputation = "first",
  flag_imputation = "auto",
  min_dates = NULL,
  max_dates = NULL,
  preserve = FALSE,
  ignore_seconds_flag = FALSE
)
}
\arguments{
\item{dataset}{Input dataset

The variables specified by the \code{dtc} argument are expected to be in the dataset.

\describe{
\item{Permitted values}{a dataset, i.e., a \code{data.frame} or tibble}
\item{Default value}{none}
}}

\item{new_vars_prefix}{Prefix used for the output variable(s).

A character scalar is expected. For the date variable (\verb{*DT}) is appended to
the specified prefix, for the date imputation flag (\verb{*DTF}), and for the time
imputation flag (\verb{*TMF}), i.e., for \code{new_vars_prefix = "AST"} the variables
\code{ASTDT}, \code{ASTDTF}, and \code{ASTTMF} are created.

\describe{
\item{Permitted values}{a character scalar, i.e., a character vector of length one}
\item{Default value}{none}
}}

\item{dtc}{The \code{--DTC} date to impute

A character date is expected in a format like \code{yyyy-mm-dd} or
\code{yyyy-mm-ddThh:mm:ss}. Trailing components can be omitted and \code{-} is a
valid "missing" value for any component.

\describe{
\item{Permitted values}{a character date variable}
\item{Default value}{none}
}}

\item{highest_imputation}{Highest imputation level

The \code{highest_imputation} argument controls which components of the DTC
value are imputed if they are missing. All components up to the specified
level are imputed.

If a component at a higher level than the highest imputation level is
missing, \code{NA_character_} is returned. For example, for \code{highest_imputation = "D"} \code{"2020"} results in \code{NA_character_} because the month is missing.

If \code{"n"} is specified, no imputation is performed, i.e., if any component is
missing, \code{NA_character_} is returned.

If \code{"Y"} is specified, \code{date_imputation} should be \code{"first"} or \code{"last"}
and \code{min_dates} or \code{max_dates} should be specified respectively. Otherwise,
\code{NA_character_} is returned if the year component is missing.

\describe{
\item{Permitted values}{\code{"Y"} (year, highest level), \code{"M"} (month), \code{"D"}
(day), \code{"h"} (hour), \code{"m"} (minute), \code{"s"} (second), \code{"n"} (none, lowest
level)}
\item{Default value}{\code{"h"}}
}}

\item{date_imputation}{The value to impute the day/month when a datepart is
missing.

A character value is expected.
\itemize{
\item If  \code{highest_imputation} is \code{"M"}, month and day can be
specified as \code{"mm-dd"}: e.g. \code{"06-15"} for the 15th of June
\item When  \code{highest_imputation} is \code{"M"} or  \code{"D"}, the following keywords are available:
\code{"first"}, \code{"mid"}, \code{"last"} to impute to the first/mid/last
day/month. If \code{"mid"} is specified, missing components are imputed as the
middle of the possible range:
\itemize{
\item If both month and day are missing, they are imputed as \code{"06-30"}
(middle of the year).
\item If only day is missing, it is imputed as \code{"15"} (middle of the month).
}
}

The year can not be specified; for imputing the year
\code{"first"} or \code{"last"} together with \code{min_dates} or \code{max_dates} argument can
be used (see examples).

\describe{
\item{Permitted values}{\code{"first"}, \code{"mid"}, \code{"last"}, or user-defined}
\item{Default value}{\code{"first"}}
}}

\item{time_imputation}{The value to impute the time when a timepart is
missing.

A character value is expected, either as a
\itemize{
\item format with hour, min and sec specified as \code{"hh:mm:ss"}: e.g. \code{"00:00:00"}
for the start of the day,
\item or as a keyword: \code{"first"},\code{"last"} to impute to the start/end of a day.
}

The argument is ignored if \code{highest_imputation = "n"}.

\describe{
\item{Permitted values}{\code{"first"}, \code{"last"}, or user-defined}
\item{Default value}{\code{"first"}}
}}

\item{flag_imputation}{Whether the date/time imputation flag(s) must also be derived.

If \code{"both"} or \code{"date"} is specified, then date imputation flag is derived.
If \code{"auto"} is specified and \code{highest_imputation} argument is greater than
\code{"h"}, then date imputation flag is derived.

If \code{"both"} or \code{"time"} is specified, then time imputation flag is derived.
If \code{"auto"} is specified and \code{highest_imputation} argument is not \code{"n"},
then time imputation flag is derived.

If \code{"none"} is specified, then no date or time imputation flag is derived.

Please note that CDISC requirements dictate the need for a date/time imputation
flag if any imputation is performed, so \code{flag_imputation = "none"} should
only be used if the imputed variable is not part of the final ADaM dataset.

\describe{
\item{Permitted values}{\code{"auto"}, \code{"date"},\code{"time"}, \code{"both"} or \code{"none"}}
\item{Default value}{\code{"auto"}}
}}

\item{min_dates}{Minimum dates

A list of dates is expected. It is ensured that the imputed date is not
before any of the specified dates, e.g., that the imputed adverse event start
date is not before the first treatment date. Only dates which are in the
range of possible dates of the \code{dtc} value are considered. The possible dates
are defined by the missing parts of the \code{dtc} date (see example below). This
ensures that the non-missing parts of the \code{dtc} date are not changed.
A date or date-time object is expected.
For example

\if{html}{\out{<div class="sourceCode r">}}\preformatted{impute_dtc_dtm(
  "2020-11",
  min_dates = list(
   ymd_hms("2020-12-06T12:12:12"),
   ymd_hms("2020-11-11T11:11:11")
  ),
  highest_imputation = "M"
)
}\if{html}{\out{</div>}}

returns \code{"2020-11-11T11:11:11"} because the possible dates for \code{"2020-11"}
range from \code{"2020-11-01T00:00:00"} to \code{"2020-11-30T23:59:59"}. Therefore
\code{"2020-12-06T12:12:12"} is ignored. Returning \code{"2020-12-06T12:12:12"} would
have changed the month although it is not missing (in the \code{dtc} date).

For date variables (not datetime) in the list the time is imputed to
\code{"00:00:00"}. Specifying date variables makes sense only if the date is
imputed. If only time is imputed, date variables do not affect the result.

\describe{
\item{Permitted values}{a list of dates, e.g. \code{list(ymd_hms("2021-07-01T04:03:01"), ymd_hms("2022-05-12T13:57:23"))}}
\item{Default value}{\code{NULL}}
}}

\item{max_dates}{Maximum dates

A list of dates is expected. It is ensured that the imputed date is not after
any of the specified dates, e.g., that the imputed date is not after the data
cut off date. Only dates which are in the range of possible dates are
considered. A date or date-time object is expected.

For date variables (not datetime) in the list the time is imputed to
\code{"23:59:59"}. Specifying date variables makes sense only if the date is
imputed. If only time is imputed, date variables do not affect the result.

\describe{
\item{Permitted values}{a list of dates, e.g. \code{list(ymd_hms("2021-07-01T04:03:01"), ymd_hms("2022-05-12T13:57:23"))}}
\item{Default value}{\code{NULL}}
}}

\item{preserve}{Preserve lower level date/time part when higher order part
is missing, e.g. preserve day if month is missing or
preserve minute when hour is missing.

For example \code{"2019---07"} would return \verb{"2019-06-07} if \code{preserve = TRUE}
(and \code{date_imputation = "mid"}).

\describe{
\item{Permitted values}{\code{"TRUE"}, \code{"FALSE"}}
\item{Default value}{\code{FALSE}}
}}

\item{ignore_seconds_flag}{ADaM IG states that given SDTM (\code{--DTC}) variable,
if only hours and minutes are ever collected, and seconds are imputed in
(\verb{*DTM}) as 00, then it is not necessary to set (\verb{*TMF}) to \code{"S"}. A user can set this
to \code{TRUE} so the \code{"S"} Flag is dropped from (\verb{*TMF}).

Please note that the default value of \code{ignore_seconds_flag} will change to \code{TRUE} in
admiral 1.4.0.

\describe{
\item{Permitted values}{\code{"TRUE"}, \code{"FALSE"}}
\item{Default value}{\code{FALSE}}
}}
}
\value{
The input dataset with the datetime \verb{*DTM} (and the date/time imputation
flag \verb{*DTF}, \verb{*TMF}) added.
}
\description{
Derive a datetime object (\verb{*DTM}) from a character date (\code{--DTC}).
The date and time can be imputed (see \code{date_imputation}/\code{time_imputation} arguments)
and the date/time imputation flag (\verb{*DTF}, \verb{*TMF}) can be added.
}
\details{
In \code{{admiral}} we don't allow users to pick any single part of the date/time to
impute, we only enable to impute up to a highest level, i.e. you couldn't
choose to say impute months, but not days.

The presence of a \verb{*DTF} variable is checked and the variable is not derived
if it already exists in the input dataset. However, if \verb{*TMF} already exists
in the input dataset, a warning is issued and \verb{*TMF} will be overwritten.
}
\seealso{
\code{vignette("imputation")}

Date/Time Derivation Functions that returns variable appended to dataset: 
\code{\link{derive_var_trtdurd}()},
\code{\link{derive_vars_dt}()},
\code{\link{derive_vars_dtm_to_dt}()},
\code{\link{derive_vars_dtm_to_tm}()},
\code{\link{derive_vars_duration}()},
\code{\link{derive_vars_dy}()}
}
\concept{der_date_time}
\keyword{der_date_time}
\keyword{der_gen}
\section{Examples}{
\subsection{Derive a datetime variable imputing time}{

In this example, we derive \code{ASTDTM} from \code{MHSTDTC}. Note that by default the function
imputes missing time components to \code{00} but doesn't impute missing date components
and automatically produces the time imputation flag (\code{ASTTMF}).

\if{html}{\out{<div class="sourceCode r">}}\preformatted{library(tibble)
library(lubridate)

mhdt <- tribble(
  ~MHSTDTC,
  "2019-07-18T15:25:40",
  "2019-07-18T15:25",
  "2019-07-18",
  "2019-02",
  "2019",
  "2019---07",
  ""
)

derive_vars_dtm(
  mhdt,
  new_vars_prefix = "AST",
  dtc = MHSTDTC
)
#> # A tibble: 7 × 3
#>   MHSTDTC               ASTDTM              ASTTMF
#>   <chr>                 <dttm>              <chr> 
#> 1 "2019-07-18T15:25:40" 2019-07-18 15:25:40 <NA>  
#> 2 "2019-07-18T15:25"    2019-07-18 15:25:00 S     
#> 3 "2019-07-18"          2019-07-18 00:00:00 H     
#> 4 "2019-02"             NA                  <NA>  
#> 5 "2019"                NA                  <NA>  
#> 6 "2019---07"           NA                  <NA>  
#> 7 ""                    NA                  <NA>  }\if{html}{\out{</div>}}}
\subsection{Impute to the latest (\code{date_imputation = "last"})}{

In this example, we set \code{date_imputation = "last"} to get the last month/day
for partial dates. We also set \code{time_imputation = "last"}. The function will use
all or part of \code{23:59:59} for time imputation. Note that \code{highest_imputation} must
be at least \code{"D"} to perform date imputation. Here we use \code{highest_imputation = "M"}
to request imputation of month and day (and time). Also note that
two flag variables are created.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{derive_vars_dtm(
 mhdt,
 new_vars_prefix = "AST",
 dtc = MHSTDTC,
 date_imputation = "last",
 time_imputation = "last",
 highest_imputation = "M"
)
#> # A tibble: 7 × 4
#>   MHSTDTC               ASTDTM              ASTDTF ASTTMF
#>   <chr>                 <dttm>              <chr>  <chr> 
#> 1 "2019-07-18T15:25:40" 2019-07-18 15:25:40 <NA>   <NA>  
#> 2 "2019-07-18T15:25"    2019-07-18 15:25:59 <NA>   S     
#> 3 "2019-07-18"          2019-07-18 23:59:59 <NA>   H     
#> 4 "2019-02"             2019-02-28 23:59:59 D      H     
#> 5 "2019"                2019-12-31 23:59:59 M      H     
#> 6 "2019---07"           2019-12-31 23:59:59 M      H     
#> 7 ""                    NA                  <NA>   <NA>  }\if{html}{\out{</div>}}}
\subsection{Suppress imputation flags (\code{flag_imputation = "none"})}{

In this example, we derive \code{ASTDTM} but suppress the \code{ASTTMF}. Note that
function appends missing \code{"hh:mm:ss"} to \code{ASTDTM}. The \code{flag_imputation = "none"}
call ensures no date/time imputation flag is created. In practice, as per CDISC
requirements this option can only be selected if the imputed variable is not part
of the final ADaM dataset.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{derive_vars_dtm(
  mhdt,
  new_vars_prefix = "AST",
  dtc = MHSTDTC,
  flag_imputation = "none"
)
#> # A tibble: 7 × 2
#>   MHSTDTC               ASTDTM             
#>   <chr>                 <dttm>             
#> 1 "2019-07-18T15:25:40" 2019-07-18 15:25:40
#> 2 "2019-07-18T15:25"    2019-07-18 15:25:00
#> 3 "2019-07-18"          2019-07-18 00:00:00
#> 4 "2019-02"             NA                 
#> 5 "2019"                NA                 
#> 6 "2019---07"           NA                 
#> 7 ""                    NA                 }\if{html}{\out{</div>}}}
\subsection{Avoid imputation after specified datetimes (\code{max_dates})}{

In this example, we derive \code{AENDTM} where AE end date is imputed to the last date.
To ensure that the imputed date is not after the death or data cut off date we can
set \code{max_dates = exprs(DTHDT, DCUTDT)}. Note two flag variables: \code{ASTDTF} and \code{ASTTMF}
are created. Setting \code{highest_imputation = "Y"} will allow for the missing \code{AEENDTC}
record to be imputed from \code{max_dates = exprs(DTHDT, DCUTDT)}.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{adae <- tribble(
   ~AEENDTC,             ~DTHDT,           ~DCUTDT,
   "2020-12", ymd("2020-12-26"), ymd("2020-12-24"),
   "2020-11", ymd("2020-12-06"), ymd("2020-12-24"),
          "", ymd("2020-12-06"), ymd("2020-12-24"),
"2020-12-20", ymd("2020-12-06"), ymd("2020-12-24")
)

derive_vars_dtm(
  adae,
  dtc = AEENDTC,
  new_vars_prefix = "AEN",
  highest_imputation = "Y",
  date_imputation = "last",
  time_imputation = "last",
  max_dates = exprs(DTHDT, DCUTDT)
)
#> # A tibble: 4 × 6
#>   AEENDTC      DTHDT      DCUTDT     AENDTM              AENDTF AENTMF
#>   <chr>        <date>     <date>     <dttm>              <chr>  <chr> 
#> 1 "2020-12"    2020-12-26 2020-12-24 2020-12-24 23:59:59 D      H     
#> 2 "2020-11"    2020-12-06 2020-12-24 2020-11-30 23:59:59 D      H     
#> 3 ""           2020-12-06 2020-12-24 2020-12-06 23:59:59 Y      H     
#> 4 "2020-12-20" 2020-12-06 2020-12-24 2020-12-20 23:59:59 <NA>   H     }\if{html}{\out{</div>}}}
\subsection{Suppress \code{"S"} for imputation flag (\code{ignore_seconds_flag})}{

In this example, we set \code{ignore_seconds_flag = TRUE} to suppress \code{S} for
seconds in the \code{ASTTMF} variable. The ADaM IG states that given SDTM (\code{--DTC})
variable, if only hours and minutes are ever collected, and seconds are imputed
in (\verb{*DTM}) as \code{00}, then it is not necessary to set (\verb{*TMF}) to \code{"S"}.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{
mhdt <- tribble(
~MHSTDTC,
"2019-07-18T15:25",
"2019-07-18",
"2019-02",
"2019",
"2019---07",
""
)

derive_vars_dtm(
  mhdt,
  new_vars_prefix = "AST",
  dtc = MHSTDTC,
  highest_imputation = "M",
  ignore_seconds_flag = TRUE
)
#> # A tibble: 6 × 4
#>   MHSTDTC            ASTDTM              ASTDTF ASTTMF
#>   <chr>              <dttm>              <chr>  <chr> 
#> 1 "2019-07-18T15:25" 2019-07-18 15:25:00 <NA>   <NA>  
#> 2 "2019-07-18"       2019-07-18 00:00:00 <NA>   H     
#> 3 "2019-02"          2019-02-01 00:00:00 D      H     
#> 4 "2019"             2019-01-01 00:00:00 M      H     
#> 5 "2019---07"        2019-01-01 00:00:00 M      H     
#> 6 ""                 NA                  <NA>   <NA>  }\if{html}{\out{</div>}}}
\subsection{Preserve lower components if higher ones were imputed (\code{preserve})}{

In this example, we impute dates as the middle month/day with \code{date_imputation = "mid"}
and impute time as last (\code{23:59:59}) with \code{time_imputation = "last"}.
We use the \code{preserve} argument to "preserve" partial dates.  For example,
\code{"2019---18T15:-:05"}, will be displayed as \code{"2019-06-18 15:59:05"} by setting
\code{preserve = TRUE}.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{mhdt <- tribble(
~MHSTDTC,
"2019-07-18T15:25",
"2019---18T15:-:05",
"2019-07-18",
"2019-02",
"2019",
"2019---07",
""
)

derive_vars_dtm(
  mhdt,
  new_vars_prefix = "AST",
  dtc = MHSTDTC,
  highest_imputation = "M",
  date_imputation = "mid",
  time_imputation = "last",
  preserve = TRUE
)
#> # A tibble: 7 × 4
#>   MHSTDTC             ASTDTM              ASTDTF ASTTMF
#>   <chr>               <dttm>              <chr>  <chr> 
#> 1 "2019-07-18T15:25"  2019-07-18 15:25:59 <NA>   S     
#> 2 "2019---18T15:-:05" 2019-06-18 15:59:05 M      M     
#> 3 "2019-07-18"        2019-07-18 23:59:59 <NA>   H     
#> 4 "2019-02"           2019-02-15 23:59:59 D      H     
#> 5 "2019"              2019-06-30 23:59:59 M      H     
#> 6 "2019---07"         2019-06-07 23:59:59 M      H     
#> 7 ""                  NA                  <NA>   <NA>  }\if{html}{\out{</div>}}}
\subsection{Further examples}{

Further example usages of this function can be found in the
\code{vignette("imputation")}.}}

