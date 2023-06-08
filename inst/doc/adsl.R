## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(admiral.test)
library(lubridate)
library(stringr)

data("admiral_dm")
data("admiral_ds")
data("admiral_ex")
data("admiral_ae")
data("admiral_lb")

dm <- convert_blanks_to_na(admiral_dm)
ds <- convert_blanks_to_na(admiral_ds)
ex <- convert_blanks_to_na(admiral_ex)
ae <- convert_blanks_to_na(admiral_ae)
lb <- convert_blanks_to_na(admiral_lb)

## ----eval=TRUE----------------------------------------------------------------
adsl <- dm %>%
  select(-DOMAIN)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(USUBJID, RFSTDTC, COUNTRY, AGE, SEX, RACE, ETHNIC, ARM, ACTARM)
)

## ----eval=TRUE----------------------------------------------------------------
adsl <- dm %>%
  mutate(TRT01P = ARM, TRT01A = ACTARM)

## ----eval=TRUE----------------------------------------------------------------
# impute start and end time of exposure to first and last respectively,
# do not impute date
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_var_trtdurd()

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(USUBJID, RFSTDTC, TRTSDTM, TRTSDT, TRTEDTM, TRTEDT, TRTDURD)
)

## ----eval=TRUE----------------------------------------------------------------
# convert character date to numeric date without imputation
ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  ds_ext,
  display_vars = exprs(USUBJID, DSCAT, DSDECOD, DSTERM, DSSTDT, DSSTDTC),
  filter = DSDECOD != "SCREEN FAILURE"
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = exprs(USUBJID, EOSDT))

## ----eval=TRUE----------------------------------------------------------------
format_eosstt <- function(x) {
  case_when(
    x %in% c("COMPLETED") ~ "COMPLETED",
    x %in% c("SCREEN FAILURE") ~ NA_character_,
    TRUE ~ "DISCONTINUED"
  )
}

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = DSCAT == "DISPOSITION EVENT",
    new_vars = exprs(EOSSTT = format_eosstt(DSDECOD)),
    missing_value = exprs(EOSSTT = "ONGOING")
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = exprs(USUBJID, EOSDT, EOSSTT))

## -----------------------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds,
    by_vars = exprs(USUBJID),
    new_vars = exprs(DCSREAS = DSDECOD, DCSREASP = DSTERM),
    filter_add = DSCAT == "DISPOSITION EVENT" &
      !(DSDECOD %in% c("SCREEN FAILURE", "COMPLETED", NA))
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = exprs(USUBJID, EOSDT, EOSSTT, DCSREAS, DCSREASP))

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
adsl <- adsl %>%
  select(-DCSREAS, -DCSREASP)

## -----------------------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds,
    by_vars = exprs(USUBJID),
    new_vars = exprs(DCSREAS = DSDECOD),
    filter_add = DSCAT == "DISPOSITION EVENT" &
      DSDECOD %notin% c("SCREEN FAILURE", "COMPLETED", NA)
  ) %>%
  derive_vars_merged(
    dataset_add = ds,
    by_vars = exprs(USUBJID),
    new_vars = exprs(DCSREASP = DSTERM),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD %in% "OTHER"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = exprs(USUBJID, EOSDT, EOSSTT, DCSREAS, DCSREASP))

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    filter_add = DSDECOD == "RANDOMIZED",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(RANDDT = DSSTDT)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = exprs(USUBJID, RANDDT))

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_dt(
    new_vars_prefix = "DTH",
    dtc = DTHDTC
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = exprs(USUBJID, TRTEDT, DTHDTC, DTHDT, DTHFL))

## ----eval=FALSE---------------------------------------------------------------
#  adsl <- adsl %>%
#    derive_vars_dt(
#      new_vars_prefix = "DTH",
#      dtc = DTHDTC,
#      date_imputation = "first"
#    )

## ----eval=TRUE----------------------------------------------------------------
src_ae <- dthcaus_source(
  dataset_name = "ae",
  filter = AEOUT == "FATAL",
  date = convert_dtc_to_dtm(AESTDTC, highest_imputation = "M"),
  mode = "first",
  dthcaus = AEDECOD
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  ae,
  display_vars = exprs(USUBJID, AESTDTC, AEENDTC, AEDECOD, AEOUT),
  filter = AEOUT == "FATAL"
)

## ----eval=TRUE----------------------------------------------------------------
src_ds <- dthcaus_source(
  dataset_name = "ds",
  filter = DSDECOD == "DEATH" & grepl("DEATH DUE TO", DSTERM),
  date = DSSTDT,
  mode = "first",
  dthcaus = "Death in DS"
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  ds,
  display_vars = exprs(USUBJID, DSDECOD, DSTERM, DSSTDTC),
  filter = DSDECOD == "DEATH"
)

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_var_dthcaus(
    src_ae, src_ds,
    source_datasets = list(ae = ae, ds = ds_ext)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(USUBJID, EOSDT, DTHDTC, DTHDT, DTHCAUS),
  filter = DTHFL == "Y"
)

## ----eval=TRUE----------------------------------------------------------------
src_ae <- dthcaus_source(
  dataset_name = "ae",
  filter = AEOUT == "FATAL",
  date = convert_dtc_to_dtm(AESTDTC, highest_imputation = "M"),
  mode = "first",
  dthcaus = AEDECOD,
  traceability_vars = exprs(DTHDOM = "AE", DTHSEQ = AESEQ)
)

src_ds <- dthcaus_source(
  dataset_name = "ds",
  filter = DSDECOD == "DEATH" & grepl("DEATH DUE TO", DSTERM),
  date = DSSTDT,
  mode = "first",
  dthcaus = DSTERM,
  traceability_vars = exprs(DTHDOM = "DS", DTHSEQ = DSSEQ)
)
adsl <- adsl %>%
  select(-DTHCAUS) %>% # remove it before deriving it again
  derive_var_dthcaus(
    src_ae, src_ds,
    source_datasets = list(ae = ae, ds = ds_ext)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(USUBJID, TRTEDT, DTHDTC, DTHDT, DTHCAUS, DTHDOM, DTHSEQ),
  filter = DTHFL == "Y"
)

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_duration(
    new_var = DTHADY,
    start_date = TRTSDT,
    end_date = DTHDT
  )

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_duration(
    new_var = LDDTHELD,
    start_date = TRTEDT,
    end_date = DTHDT,
    add_one = FALSE
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(USUBJID, TRTEDT, DTHDTC, DTHDT, DTHCAUS, DTHADY, LDDTHELD),
  filter = DTHFL == "Y"
)

## ----eval=TRUE----------------------------------------------------------------
ae_start_date <- date_source(
  dataset_name = "ae",
  date = convert_dtc_to_dt(AESTDTC, highest_imputation = "M")
)
ae_end_date <- date_source(
  dataset_name = "ae",
  date = convert_dtc_to_dt(AEENDTC, highest_imputation = "M")
)
lb_date <- date_source(
  dataset_name = "lb",
  date = convert_dtc_to_dt(LBDTC, highest_imputation = "M")
)
trt_end_date <- date_source(
  dataset_name = "adsl",
  date = TRTEDT
)

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_var_extreme_dt(
    new_var = LSTALVDT,
    ae_start_date, ae_end_date, lb_date, trt_end_date,
    source_datasets = list(ae = ae, adsl = adsl, lb = lb),
    mode = "last"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(USUBJID, TRTEDT, DTHDTC, LSTALVDT),
  filter = !is.na(TRTSDT)
)

## ----eval=TRUE----------------------------------------------------------------
ae_start_date <- date_source(
  dataset_name = "ae",
  date = convert_dtc_to_dt(AESTDTC, highest_imputation = "M"),
  traceability_vars = exprs(LALVDOM = "AE", LALVSEQ = AESEQ, LALVVAR = "AESTDTC")
)
ae_end_date <- date_source(
  dataset_name = "ae",
  date = convert_dtc_to_dt(AEENDTC, highest_imputation = "M"),
  traceability_vars = exprs(LALVDOM = "AE", LALVSEQ = AESEQ, LALVVAR = "AEENDTC")
)
lb_date <- date_source(
  dataset_name = "lb",
  date = convert_dtc_to_dt(LBDTC, highest_imputation = "M"),
  traceability_vars = exprs(LALVDOM = "LB", LALVSEQ = LBSEQ, LALVVAR = "LBDTC")
)
trt_end_date <- date_source(
  dataset_name = "adsl",
  date = TRTEDTM,
  traceability_vars = exprs(LALVDOM = "ADSL", LALVSEQ = NA_integer_, LALVVAR = "TRTEDTM")
)

adsl <- adsl %>%
  select(-LSTALVDT) %>% # created in the previous call
  derive_var_extreme_dt(
    new_var = LSTALVDT,
    ae_start_date, ae_end_date, lb_date, trt_end_date,
    source_datasets = list(ae = ae, adsl = adsl, lb = lb),
    mode = "last"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(USUBJID, TRTEDT, DTHDTC, LSTALVDT, LALVDOM, LALVSEQ, LALVVAR),
  filter = !is.na(TRTSDT)
)

## ----eval=TRUE----------------------------------------------------------------
format_agegr1 <- function(var_input) {
  case_when(
    var_input < 18 ~ "<18",
    between(var_input, 18, 64) ~ "18-64",
    var_input > 64 ~ ">64",
    TRUE ~ "Missing"
  )
}

format_region1 <- function(var_input) {
  case_when(
    var_input %in% c("CAN", "USA") ~ "North America",
    !is.na(var_input) ~ "Rest of the World",
    TRUE ~ "Missing"
  )
}

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  mutate(
    AGEGR1 = format_agegr1(AGE),
    REGION1 = format_region1(COUNTRY)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(USUBJID, AGE, SEX, COUNTRY, AGEGR1, REGION1)
)

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(USUBJID, TRTSDT, ARM, ACTARM, SAFFL)
)

