## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)

ae <- pharmaversesdtm::ae
adsl <- admiral::admiral_adsl
ex_single <- admiral::ex_single

ae <- convert_blanks_to_na(ae)

## ----echo = FALSE-------------------------------------------------------------
ae <- filter(ae, USUBJID %in% c("01-701-1015", "01-701-1023", "01-703-1086", "01-703-1096", "01-707-1037", "01-716-1024"))

## ----eval=TRUE----------------------------------------------------------------
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P, DTHDT, EOSDT)

adae <- derive_vars_merged(
  ae,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by = exprs(STUDYID, USUBJID)
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adae,
  display_vars = exprs(
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
    min_dates = exprs(TRTSDT)
  ) %>%
  derive_vars_dtm(
    dtc = AEENDTC,
    new_vars_prefix = "AEN",
    highest_imputation = "M",
    date_imputation = "last",
    time_imputation = "last",
    max_dates = exprs(DTHDT, EOSDT)
  ) %>%
  derive_vars_dtm_to_dt(exprs(ASTDTM, AENDTM)) %>%
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = exprs(ASTDT, AENDT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adae,
  display_vars = exprs(
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

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adae,
  display_vars = exprs(
    USUBJID, AESTDTC, AEENDTC, ASTDT, AENDT,
    ADURN, ADURU
  )
)

## ----eval=TRUE----------------------------------------------------------------
cm <- tibble::tribble(
  ~STUDYID,  ~USUBJID,       ~CMGRPID, ~CMREFID,  ~CMDECOD,
  "STUDY01", "BP40257-1001", "14",     "1192056", "PARACETAMOL",
  "STUDY01", "BP40257-1001", "18",     "2007001", "SOLUMEDROL",
  "STUDY01", "BP40257-1002", "19",     "2791596", "SPIRONOLACTONE"
)
facm <- tibble::tribble(
  ~STUDYID,  ~USUBJID,       ~FAGRPID, ~FAREFID,  ~FATESTCD,  ~FASTRESC,
  "STUDY01", "BP40257-1001", "1",      "1192056", "CMATC1CD", "N",
  "STUDY01", "BP40257-1001", "1",      "1192056", "CMATC2CD", "N02",
  "STUDY01", "BP40257-1001", "1",      "1192056", "CMATC3CD", "N02B",
  "STUDY01", "BP40257-1001", "1",      "1192056", "CMATC4CD", "N02BE",
  "STUDY01", "BP40257-1001", "1",      "2007001", "CMATC1CD", "D",
  "STUDY01", "BP40257-1001", "1",      "2007001", "CMATC2CD", "D10",
  "STUDY01", "BP40257-1001", "1",      "2007001", "CMATC3CD", "D10A",
  "STUDY01", "BP40257-1001", "1",      "2007001", "CMATC4CD", "D10AA",
  "STUDY01", "BP40257-1001", "2",      "2007001", "CMATC1CD", "D",
  "STUDY01", "BP40257-1001", "2",      "2007001", "CMATC2CD", "D07",
  "STUDY01", "BP40257-1001", "2",      "2007001", "CMATC3CD", "D07A",
  "STUDY01", "BP40257-1001", "2",      "2007001", "CMATC4CD", "D07AA",
  "STUDY01", "BP40257-1001", "3",      "2007001", "CMATC1CD", "H",
  "STUDY01", "BP40257-1001", "3",      "2007001", "CMATC2CD", "H02",
  "STUDY01", "BP40257-1001", "3",      "2007001", "CMATC3CD", "H02A",
  "STUDY01", "BP40257-1001", "3",      "2007001", "CMATC4CD", "H02AB",
  "STUDY01", "BP40257-1002", "1",      "2791596", "CMATC1CD", "C",
  "STUDY01", "BP40257-1002", "1",      "2791596", "CMATC2CD", "C03",
  "STUDY01", "BP40257-1002", "1",      "2791596", "CMATC3CD", "C03D",
  "STUDY01", "BP40257-1002", "1",      "2791596", "CMATC4CD", "C03DA"
)

derive_vars_atc(cm, dataset_facm = facm, id_vars = exprs(FAGRPID))

## ----eval=TRUE----------------------------------------------------------------
adae <- mutate(adae, TRTP = TRT01P, TRTA = TRT01A)

count(adae, TRTP, TRTA, TRT01P, TRT01A)

## ----eval=TRUE----------------------------------------------------------------
ex_single <- derive_vars_dtm(
  ex_single,
  dtc = EXSTDTC,
  new_vars_prefix = "EXST",
  flag_imputation = "none"
)

adae <- derive_vars_joined(
  adae,
  ex_single,
  by_vars = exprs(STUDYID, USUBJID),
  new_vars = exprs(LDOSEDTM = EXSTDTM),
  join_vars = exprs(EXSTDTM),
  join_type = "all",
  order = exprs(EXSTDTM),
  filter_add = (EXDOSE > 0 | (EXDOSE == 0 & grepl("PLACEBO", EXTRT))) & !is.na(EXSTDTM),
  filter_join = EXSTDTM <= ASTDTM,
  mode = "last"
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adae,
  display_vars = exprs(
    USUBJID, AEDECOD, AESEQ, AESTDTC, AEENDTC,
    ASTDT, AENDT, LDOSEDTM
  )
)

## ----eval=TRUE----------------------------------------------------------------
ex_single <- derive_vars_dtm(
  ex_single,
  dtc = EXENDTC,
  new_vars_prefix = "EXEN",
  time_imputation = "last",
  flag_imputation = "none"
)

adae <- derive_vars_joined(
  adae,
  ex_single,
  by_vars = exprs(STUDYID, USUBJID),
  new_vars = exprs(DOSEON = EXDOSE, DOSEU = EXDOSU),
  join_vars = exprs(EXSTDTM, EXENDTM),
  join_type = "all",
  filter_add = (EXDOSE > 0 | (EXDOSE == 0 & grepl("PLACEBO", EXTRT))) & !is.na(EXSTDTM),
  filter_join = EXSTDTM <= ASTDTM & (ASTDTM <= EXENDTM | is.na(EXENDTM))
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adae,
  display_vars = exprs(
    USUBJID, AEDECOD, AESEQ, AESTDTC, AEENDTC,
    ASTDT, AENDT, DOSEON, DOSEU
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
  derive_var_trtemfl(
    trt_start_date = TRTSDT,
    trt_end_date = TRTEDT,
    end_window = 30
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adae,
  display_vars = exprs(
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

## ----eval=FALSE---------------------------------------------------------------
# adae <- adae %>%
#   restrict_derivation(
#     derivation = derive_var_extreme_flag,
#     args = params(
#       by_vars = exprs(USUBJID),
#       order = exprs(desc(ATOXGR), ASTDTM, AESEQ),
#       new_var = AOCCIFL,
#       mode = "first"
#     ),
#     filter = TRTEMFL == "Y"
#   )

## ----eval=TRUE----------------------------------------------------------------
adae <- adae %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID),
      order = exprs(
        as.integer(factor(
          ASEV,
          levels = c("DEATH THREATENING", "SEVERE", "MODERATE", "MILD")
        )),
        ASTDTM, AESEQ
      ),
      new_var = AOCCIFL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adae,
  display_vars = exprs(
    USUBJID, ASTDTM, ASEV, AESEQ, TRTEMFL, AOCCIFL
  )
)

## ----eval=TRUE----------------------------------------------------------------
queries <- admiral::queries

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(queries)

## ----eval=TRUE----------------------------------------------------------------
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

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(adae_query)

## ----eval=TRUE----------------------------------------------------------------
sdg <- tibble::tribble(
  ~PREFIX, ~GRPNAME,          ~GRPID, ~SCOPE,  ~SCOPEN, ~SRCVAR,   ~TERMCHAR,          ~TERMNUM,
  "SDG01", "Diuretics",           11, "BROAD", 1,       "CMDECOD", "Diuretic 1",       NA,
  "SDG01", "Diuretics",           11, "BROAD", 1,       "CMDECOD", "Diuretic 2",       NA,
  "SDG02", "Costicosteroids",     12, "BROAD", 1,       "CMDECOD", "Costicosteroid 1", NA,
  "SDG02", "Costicosteroids",     12, "BROAD", 1,       "CMDECOD", "Costicosteroid 2", NA,
  "SDG02", "Costicosteroids",     12, "BROAD", 1,       "CMDECOD", "Costicosteroid 3", NA,
)
adcm <- tibble::tribble(
  ~USUBJID, ~ASTDTM,               ~CMDECOD,
  "01",     "2020-06-02 23:59:59", "Diuretic 1",
  "02",     "2020-06-05 23:59:59", "Diuretic 1",
  "03",     "2020-06-07 23:59:59", "Costicosteroid 2",
  "05",     "2020-06-09 23:59:59", "Diuretic 2"
)
adcm_query <- derive_vars_query(adcm, sdg)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(adcm_query)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adae <- adae %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adae,
  display_vars = exprs(
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
    by_vars    = exprs(USUBJID),
    order      = exprs(ASTDTM, CMSEQ, ATC1CD, ATC2CD, ATC3CD, ATC4CD),
    new_var    = ASEQ,
    check_type = "error"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(adcm_aseq)

