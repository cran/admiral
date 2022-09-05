## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(admiral)
library(dplyr)
library(admiral.test)
library(lubridate)

data("admiral_ae")
data("admiral_adsl")

ae <- admiral_ae
adsl <- admiral_adsl

## ----echo = FALSE-------------------------------------------------------------
ae <- filter(ae, USUBJID %in% c("01-701-1015", "01-701-1023", "01-703-1086", "01-703-1096", "01-707-1037", "01-716-1024"))


## ----eval=TRUE----------------------------------------------------------------

adsl_vars <- vars(TRTSDT, TRTEDT, TRT01A, TRT01P, DTHDT, EOSDT)

adae <- derive_vars_merged(
  ae,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by = vars(STUDYID, USUBJID)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adae,
  display_vars = vars(
    USUBJID, AESEQ, AETERM, AESTDTC, TRTSDT,
    TRTEDT, TRT01A, TRT01P, DTHDT, EOSDT
  )
)

## ----eval=TRUE----------------------------------------------------------------
adae <- adae %>%
  derive_vars_dtm(
    dtc = AESTDTC,
    new_vars_prefix = "AST",
    highest_imputation = "M",
    min_dates = vars(TRTSDT)
  ) %>%
  derive_vars_dtm(
    dtc = AEENDTC,
    new_vars_prefix = "AEN",
    highest_imputation = "M",
    date_imputation = "last",
    time_imputation = "last",
    max_dates = vars(DTHDT, EOSDT)
  ) %>%
  derive_vars_dtm_to_dt(vars(ASTDTM, AENDTM)) %>%
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = vars(ASTDT, AENDT)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adae,
  display_vars = vars(
    USUBJID, AESTDTC, AEENDTC, ASTDTM, ASTDT,
    ASTDY, AENDTM, AENDT, AENDY
  )
)

## ----eval=TRUE----------------------------------------------------------------
adae <- adae %>%
  derive_vars_duration(
    new_var = ADURN,
    new_var_unit = ADURU,
    start_date = ASTDT,
    end_date = AENDT
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adae,
  display_vars = vars(
    USUBJID, AESTDTC, AEENDTC, ASTDT, AENDT,
    ADURN, ADURU
  )
)

## ----eval=TRUE----------------------------------------------------------------
cm <- tibble::tribble(
  ~USUBJID,       ~CMGRPID, ~CMREFID,  ~CMDECOD,
  "BP40257-1001",     "14", "1192056", "PARACETAMOL",
  "BP40257-1001",     "18", "2007001", "SOLUMEDROL",
  "BP40257-1002",     "19", "2791596", "SPIRONOLACTONE"
)
facm <- tibble::tribble(
  ~USUBJID,       ~FAGRPID,  ~FAREFID, ~FATESTCD,  ~FASTRESC,
  "BP40257-1001",      "1", "1192056", "CMATC1CD",       "N",
  "BP40257-1001",      "1", "1192056", "CMATC2CD",     "N02",
  "BP40257-1001",      "1", "1192056", "CMATC3CD",    "N02B",
  "BP40257-1001",      "1", "1192056", "CMATC4CD",   "N02BE",
  "BP40257-1001",      "1", "2007001", "CMATC1CD",       "D",
  "BP40257-1001",      "1", "2007001", "CMATC2CD",     "D10",
  "BP40257-1001",      "1", "2007001", "CMATC3CD",    "D10A",
  "BP40257-1001",      "1", "2007001", "CMATC4CD",   "D10AA",
  "BP40257-1001",      "2", "2007001", "CMATC1CD",       "D",
  "BP40257-1001",      "2", "2007001", "CMATC2CD",     "D07",
  "BP40257-1001",      "2", "2007001", "CMATC3CD",    "D07A",
  "BP40257-1001",      "2", "2007001", "CMATC4CD",   "D07AA",
  "BP40257-1001",      "3", "2007001", "CMATC1CD",       "H",
  "BP40257-1001",      "3", "2007001", "CMATC2CD",     "H02",
  "BP40257-1001",      "3", "2007001", "CMATC3CD",    "H02A",
  "BP40257-1001",      "3", "2007001", "CMATC4CD",   "H02AB",
  "BP40257-1002",      "1", "2791596", "CMATC1CD",       "C",
  "BP40257-1002",      "1", "2791596", "CMATC2CD",     "C03",
  "BP40257-1002",      "1", "2791596", "CMATC3CD",    "C03D",
  "BP40257-1002",      "1", "2791596", "CMATC4CD",   "C03DA"
)

derive_vars_atc(cm, facm)

## ----eval=TRUE----------------------------------------------------------------
adae <- mutate(adae, TRTP = TRT01P, TRTA = TRT01A)

count(adae, TRTP, TRTA, TRT01P, TRT01A)

## ----eval=TRUE----------------------------------------------------------------
data(ex_single)
ex_single <- derive_vars_dtm(
  ex_single,
  dtc = EXSTDTC,
  new_vars_prefix = "EXST",
  flag_imputation = "none"
)
adae <- adae %>%
  derive_var_last_dose_date(
    ex_single,
    filter_ex = (EXDOSE > 0 | (EXDOSE == 0 & grepl("PLACEBO", EXTRT))) &
      !is.na(EXSTDTM),
    dose_date = EXSTDTM,
    analysis_date = ASTDT,
    single_dose_condition = (EXSTDTC == EXENDTC),
    new_var = LDOSEDTM,
    output_datetime = TRUE
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adae,
  display_vars = vars(
    USUBJID, AEDECOD, AESEQ, AESTDTC, AEENDTC,
    ASTDT, AENDT, LDOSEDTM
  )
)

## ----eval=TRUE----------------------------------------------------------------
adae <- adae %>%
  mutate(
    ASEV = AESEV,
    AREL = AEREL
  )

## ----eval=TRUE----------------------------------------------------------------
adae <- adae %>%
  mutate(
    TRTEMFL = ifelse(ASTDT >= TRTSDT & ASTDT <= TRTEDT + days(30), "Y", NA_character_)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adae,
  display_vars = vars(
    USUBJID, TRTSDT, TRTEDT, AESTDTC, ASTDT,
    TRTEMFL
  )
)

## ----eval=TRUE----------------------------------------------------------------
bds1 <- tibble::tribble(
  ~USUBJID, ~ADT,              ~TRTSDT,           ~TRTEDT,
  "P01",    ymd("2020-02-24"), ymd("2020-01-01"), ymd("2020-03-01"),
  "P02",    ymd("2020-01-01"), ymd("2020-01-01"), ymd("2020-03-01"),
  "P03",    ymd("2019-12-31"), ymd("2020-01-01"), ymd("2020-03-01")
)
derive_var_ontrtfl(
  bds1,
  start_date = ADT,
  ref_start_date = TRTSDT,
  ref_end_date = TRTEDT
)

bds2 <- tibble::tribble(
  ~USUBJID, ~ADT,              ~TRTSDT,           ~TRTEDT,
  "P01",    ymd("2020-07-01"), ymd("2020-01-01"), ymd("2020-03-01"),
  "P02",    ymd("2020-04-30"), ymd("2020-01-01"), ymd("2020-03-01"),
  "P03",    ymd("2020-03-15"), ymd("2020-01-01"), ymd("2020-03-01")
)
derive_var_ontrtfl(
  bds2,
  start_date = ADT,
  ref_start_date = TRTSDT,
  ref_end_date = TRTEDT,
  ref_end_window = 60
)

bds3 <- tibble::tribble(
  ~ADTM,              ~TRTSDTM,           ~TRTEDTM,           ~TPT,
  "2020-01-02T12:00", "2020-01-01T12:00", "2020-03-01T12:00", NA,
  "2020-01-01T12:00", "2020-01-01T12:00", "2020-03-01T12:00", "PRE",
  "2019-12-31T12:00", "2020-01-01T12:00", "2020-03-01T12:00", NA
) %>%
  mutate(
    ADTM = ymd_hm(ADTM),
    TRTSDTM = ymd_hm(TRTSDTM),
    TRTEDTM = ymd_hm(TRTEDTM)
  )
derive_var_ontrtfl(
  bds3,
  start_date = ADTM,
  ref_start_date = TRTSDTM,
  ref_end_date = TRTEDTM,
  filter_pre_timepoint = TPT == "PRE"
)

## ---- eval=FALSE--------------------------------------------------------------
#  adae <- adae %>%
#    restrict_derivation(
#      derivation = derive_var_extreme_flag,
#      args = params(
#        by_vars = vars(USUBJID),
#        order = vars(desc(ATOXGR), ASTDTM, AESEQ),
#        new_var = AOCCIFL,
#        mode = "first"
#      ),
#      filter = TRTEMFL == "Y"
#    )

## ---- eval=TRUE---------------------------------------------------------------
adae <- adae %>%
  mutate(
    ASEVN = as.integer(factor(ASEV, levels = c("MILD", "MODERATE", "SEVERE", "DEATH THREATENING")))
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(desc(ASEVN), ASTDTM, AESEQ),
      new_var = AOCCIFL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adae,
  display_vars = vars(
    USUBJID, ASTDTM, ASEV, ASEVN, AESEQ, TRTEMFL,
    AOCCIFL
  )
)

## ---- eval=TRUE---------------------------------------------------------------
data("queries")

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(queries)

## ---- eval=TRUE---------------------------------------------------------------
adae1 <- tibble::tribble(
  ~USUBJID, ~ASTDTM, ~AETERM, ~AESEQ, ~AEDECOD, ~AELLT, ~AELLTCD,
  "01", "2020-06-02 23:59:59", "ALANINE AMINOTRANSFERASE ABNORMAL",
  3, "Alanine aminotransferase abnormal", NA_character_, NA_integer_,
  "02", "2020-06-05 23:59:59", "BASEDOW'S DISEASE",
  5, "Basedow's disease", NA_character_, 1L,
  "03", "2020-06-07 23:59:59", "SOME TERM",
  2, "Some query", "Some term", NA_integer_,
  "05", "2020-06-09 23:59:59", "ALVEOLAR PROTEINOSIS",
  7, "Alveolar proteinosis", NA_character_, NA_integer_
)

adae_query <- derive_vars_query(dataset = adae1, dataset_queries = queries)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adae_query)

## ---- eval=TRUE---------------------------------------------------------------
sdg <- tibble::tribble(
  ~VAR_PREFIX, ~QUERY_NAME,       ~SDG_ID, ~QUERY_SCOPE, ~QUERY_SCOPE_NUM, ~TERM_LEVEL, ~TERM_NAME,         ~TERM_ID,
  "SDG01",     "Diuretics",       11,      "BROAD",      1,                "CMDECOD",   "Diuretic 1",       NA,
  "SDG01",     "Diuretics",       11,      "BROAD",      2,                "CMDECOD",   "Diuretic 2",       NA,
  "SDG02",     "Costicosteroids", 12,      "BROAD",      1,                "CMDECOD",   "Costicosteroid 1", NA,
  "SDG02",     "Costicosteroids", 12,      "BROAD",      2,                "CMDECOD",   "Costicosteroid 2", NA,
  "SDG02",     "Costicosteroids", 12,      "BROAD",      2,                "CMDECOD",   "Costicosteroid 3", NA,
)
adcm <- tibble::tribble(
  ~USUBJID, ~ASTDTM,               ~CMDECOD,
  "01",     "2020-06-02 23:59:59", "Diuretic 1",
  "02",     "2020-06-05 23:59:59", "Diuretic 1",
  "03",     "2020-06-07 23:59:59", "Costicosteroid 2",
  "05",     "2020-06-09 23:59:59", "Diuretic 2"
)
adcm_query <- derive_vars_query(adcm, sdg)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adcm_query)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adae <- adae %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = vars(STUDYID, USUBJID)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adae,
  display_vars = vars(
    USUBJID, AEDECOD, ASTDTM, DTHDT, RFSTDTC,
    RFENDTC, AGE, AGEU, SEX
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adcm <- tibble::tribble(
  ~USUBJID,       ~ASTDTM,          ~CMSEQ, ~CMDECOD,         ~ATC1CD, ~ATC2CD, ~ATC3CD, ~ATC4CD,
  "BP40257-1001", "2013-07-05 UTC", "14",   "PARACETAMOL",    "N",     "N02",   "N02B",  "N02BE",
  "BP40257-1001", "2013-08-15 UTC", "18",   "SOLUMEDROL",     "D",     "D10",   "D10A",  "D10AA",
  "BP40257-1001", "2013-08-15 UTC", "18",   "SOLUMEDROL",     "D",     "D07",   "D07A",  "D07AA",
  "BP40257-1001", "2013-08-15 UTC", "18",   "SOLUMEDROL",     "H",     "H02",   "H02A",  "H02AB",
  "BP40257-1002", "2012-12-15 UTC", "19",   "SPIRONOLACTONE", "C",     "C03",   "C03D",  "C03DA"
)

adcm_aseq <- adcm %>%
  derive_var_obs_number(
    by_vars    = vars(USUBJID),
    order      = vars(ASTDTM, CMSEQ, ATC1CD, ATC2CD, ATC3CD, ATC4CD),
    new_var    = ASEQ,
    check_type = "error"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adcm_aseq)

