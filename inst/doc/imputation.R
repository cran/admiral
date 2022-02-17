## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(admiral)
library(lubridate)
library(tibble)
library(dplyr)

## -----------------------------------------------------------------------------
impute_dtc(
  "2019-10",
  date_imputation = "01-01",
  time_imputation = "00:00:00"
)

## -----------------------------------------------------------------------------
impute_dtc(
  "2019-02",
  date_imputation = "02-31",
  time_imputation = "00:00:00"
)

## -----------------------------------------------------------------------------
impute_dtc(
  "2019-02",
  date_imputation = "last",
  time_imputation = "00:00:00"
)

## -----------------------------------------------------------------------------
impute_dtc(
  "2019-02",
  date_imputation = "last",
  time_imputation = "last",
  max_dates = list(ymd("2019-01-14"), ymd("2019-02-25"))
)

## -----------------------------------------------------------------------------
ae <- tribble(~ AESTDTC,
              "2019-08-09T12:34:56",
              "2019-04-12",
              "2010-09",
              NA_character_) %>%
  derive_vars_dtm(
    dtc = AESTDTC,
    new_vars_prefix = "AST",
    date_imputation = "first",
    time_imputation = "first"
  ) %>%
  mutate(ASTDT = date(ASTDTM))

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(ae)

## -----------------------------------------------------------------------------
ae <- tribble(~ AESTDTC,
              "2019-08-09T12:34:56",
              "2019-04-12",
              "2010-09",
              NA_character_) %>%
  derive_vars_dt(dtc = AESTDTC,
                 new_vars_prefix = "AST",
                 date_imputation = "first")

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(ae)

## -----------------------------------------------------------------------------
ae <- tribble(~ AESTDTC,
              "2019-08-09T12:34:56",
              "2019-04-12",
              "2010-09",
              NA_character_) %>%
  derive_vars_dtm(
    dtc = AESTDTC,
    new_vars_prefix = "AST",
    date_imputation = NULL,
    time_imputation = "first"
  )

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(ae)

## -----------------------------------------------------------------------------
ae <- tribble(
  ~AESTDTC,              ~TRTSDTM,
  "2019-08-09T12:34:56", ymd_hms("2019-11-11T12:34:56"),
  "2019-10",             ymd_hms("2019-11-11T12:34:56"),
  "2019-11",             ymd_hms("2019-11-11T12:34:56"),
  "2019-12-04",          ymd_hms("2019-11-11T12:34:56")
) %>%
  derive_vars_dtm(
    dtc = AESTDTC,
    new_vars_prefix = "AST",
    date_imputation = "first",
    time_imputation = "first",
    min_dates = vars(TRTSDTM)
  )

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(ae)

## -----------------------------------------------------------------------------
ae <- tribble(
  ~AEENDTC,              ~DTHDT,            ~DCUTDT,
  "2019-08-09T12:34:56", ymd("2019-11-11"), ymd("2019-12-02"),
  "2019-11",             ymd("2019-11-11"), ymd("2019-12-02"),
  "2019-12",             NA,                ymd("2019-12-02"),
  "2019-12-04",          NA,                ymd("2019-12-02")
) %>%
  derive_vars_dtm(
    dtc = AEENDTC,
    new_vars_prefix = "AEN",
    date_imputation = "last",
    time_imputation = "last",
    max_dates = vars(DTHDT, DCUTDT)
  )

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(ae)

## -----------------------------------------------------------------------------
mh <- tribble(
  ~MHSTDTC,     ~TRTSDT,
  "2019-04",    ymd("2019-04-15"),
  "2019-04-01", ymd("2019-04-15"),
  "2019-05",    ymd("2019-04-15"),
  "2019-06-21", ymd("2019-04-15")
) %>% 
filter(
  convert_dtc_to_dt(MHSTDTC, date_imputation = "first") < TRTSDT
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(mh)

## -----------------------------------------------------------------------------
vs <- tribble(
  ~VSDTC,                ~VSTPT,
  "2019-08-09T12:34:56", NA,
  "2019-10-12",          "PRE-DOSE",
  "2019-11-10",          NA,
  "2019-12-04",          NA
) %>% 
mutate(
  ADTM = if_else(
    VSTPT == "PRE-DOSE",
    convert_dtc_to_dtm(
      dtc = VSDTC,
      date_imputation = NULL,
      time_imputation = "first"
    ),
    convert_dtc_to_dtm(
      dtc = VSDTC,
      date_imputation = NULL,
      time_imputation = "last"
    )
  ),
  ADTMF = compute_tmf(
    dtc = VSDTC,
    dtm = ADTM
  )
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(vs)

