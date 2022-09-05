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
library(stringr)

data("admiral_dm")
data("admiral_ds")
data("admiral_ex")
data("admiral_ae")
data("admiral_lb")

dm <- admiral_dm
ds <- admiral_ds
ex <- admiral_ex
ae <- admiral_ae
lb <- admiral_lb

## ----eval=TRUE----------------------------------------------------------------
adsl <- dm %>%
  select(-DOMAIN)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = vars(USUBJID, RFSTDTC, COUNTRY, AGE, SEX, RACE, ETHNIC, ARM, ACTARM)
)

## ----eval=TRUE----------------------------------------------------------------
adsl <- dm %>%
  mutate(TRT01P = ARM, TRT01A = ACTARM)

## ----eval=TRUE----------------------------------------------------------------
# impute start and end time of exposure to first and last respectively, do not impute date
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
    new_vars = vars(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = vars(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = vars(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = vars(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = vars(STUDYID, USUBJID)
  )

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM, TRTEDTM))

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_var_trtdurd()

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = vars(USUBJID, RFSTDTC, TRTSDTM, TRTSDT, TRTEDTM, TRTEDT, TRTDURD)
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
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  ds,
  display_vars = vars(USUBJID, DSCAT, DSDECOD, DSTERM, DSSTDTC),
  filter = DSDECOD != "SCREEN FAILURE"
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = vars(USUBJID, EOSDT))

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_var_disposition_status(
    dataset_ds = ds,
    new_var = EOSSTT,
    status_var = DSDECOD,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = vars(USUBJID, EOSDT, EOSSTT))

## ----eval=TRUE----------------------------------------------------------------
format_eosstt <- function(DSDECOD) {
  case_when(
    DSDECOD %in% c("COMPLETED") ~ "COMPLETED",
    DSDECOD %in% c("SCREEN FAILURE") ~ NA_character_,
    !is.na(DSDECOD) ~ "DISCONTINUED",
    TRUE ~ "ONGOING"
  )
}

## ----eval=FALSE---------------------------------------------------------------
#  
#  adsl <- adsl %>%
#    derive_var_disposition_status(
#      dataset_ds = ds,
#      new_var = EOSSTT,
#      status_var = DSDECOD,
#      format_new_var = format_eosstt,
#      filter_ds = DSCAT == "DISPOSITION EVENT"
#    )

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_disposition_reason(
    dataset_ds = ds,
    new_var = DCSREAS,
    reason_var = DSDECOD,
    new_var_spe = DCSREASP,
    reason_var_spe = DSTERM,
    filter_ds = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = vars(USUBJID, EOSDT, EOSSTT, DCSREAS, DCSREASP))

## ----eval=TRUE----------------------------------------------------------------
format_dcsreas <- function(dsdecod, dsterm = NULL) {
  if (is.null(dsterm)) {
    if_else(dsdecod %notin% c("COMPLETED", "SCREEN FAILURE") & !is.na(dsdecod), dsdecod, NA_character_)
  } else {
    if_else(dsdecod == "OTHER", dsterm, NA_character_)
  }
}

## ----eval=FALSE---------------------------------------------------------------
#  adsl <- adsl %>%
#    derive_vars_disposition_reason(
#      dataset_ds = ds,
#      new_var = DCSREAS,
#      reason_var = DSDECOD,
#      new_var_spe = DCSREASP,
#      reason_var_spe = DSTERM,
#      format_new_vars = format_dcsreas,
#      filter_ds = DSCAT == "DISPOSITION EVENT"
#    )

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    filter_add = DSDECOD == "RANDOMIZED",
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(RANDDT = DSSTDT)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = vars(USUBJID, RANDDT))

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_vars_dt(
    new_vars_prefix = "DTH",
    dtc = DTHDTC
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(adsl, display_vars = vars(USUBJID, TRTEDT, DTHDTC, DTHDT, DTHFL))

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
  date = AESTDTM,
  mode = "first",
  dthcaus = AEDECOD
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  ae,
  display_vars = vars(USUBJID, AESTDTC, AEENDTC, AEDECOD, AEOUT),
  filter =  AEOUT == "FATAL"
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
  display_vars = vars(USUBJID, DSDECOD, DSTERM, DSSTDTC),
  filter = DSDECOD == "DEATH"
)

## ----eval=TRUE----------------------------------------------------------------
ae_ext <- derive_vars_dtm(
  ae,
  dtc = AESTDTC,
  new_vars_prefix = "AEST",
  highest_imputation = "M",
  flag_imputation = "none"
)

adsl <- adsl %>%
  derive_var_dthcaus(src_ae, src_ds, source_datasets = list(ae = ae_ext, ds = ds_ext))

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = vars(USUBJID, EOSDT, DTHDTC, DTHDT, DTHCAUS),
  filter = DTHFL == "Y"
)

## ----eval=TRUE----------------------------------------------------------------
src_ae <- dthcaus_source(
  dataset_name = "ae",
  filter = AEOUT == "FATAL",
  date = AESTDTM,
  mode = "first",
  dthcaus = AEDECOD,
  traceability_vars = vars(DTHDOM = "AE", DTHSEQ = AESEQ)
)

src_ds <- dthcaus_source(
  dataset_name = "ds",
  filter = DSDECOD == "DEATH" & grepl("DEATH DUE TO", DSTERM),
  date = DSSTDT,
  mode = "first",
  dthcaus = DSTERM,
  traceability_vars = vars(DTHDOM = "DS", DTHSEQ = DSSEQ)
)
adsl <- adsl %>%
  select(-DTHCAUS) %>% # remove it before deriving it again
  derive_var_dthcaus(src_ae, src_ds, source_datasets = list(ae = ae_ext, ds = ds_ext))

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = vars(USUBJID, TRTEDT, DTHDTC, DTHDT, DTHCAUS, DTHDOM, DTHSEQ),
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
  display_vars = vars(USUBJID, TRTEDT, DTHDTC, DTHDT, DTHCAUS, DTHADY, LDDTHELD),
  filter = DTHFL == "Y"
)

## ----eval=TRUE----------------------------------------------------------------
ae_start_date <- date_source(
  dataset_name = "ae",
  date = AESTDT
)
ae_end_date <- date_source(
  dataset_name = "ae",
  date = AEENDT
)
lb_date <- date_source(
  dataset_name = "lb",
  date = LBDT,
  filter = !is.na(LBDT)
)
trt_end_date <- date_source(
  dataset_name = "adsl",
  date = TRTEDT
)

## ----eval=TRUE----------------------------------------------------------------
# impute AE start and end date to first
ae_ext <- ae %>%
  derive_vars_dt(
    dtc = AESTDTC,
    new_vars_prefix = "AEST",
    highest_imputation = "M"
  ) %>%
  derive_vars_dt(
    dtc = AEENDTC,
    new_vars_prefix = "AEEN",
    highest_imputation = "M"
  )

# impute LB date to first
lb_ext <- derive_vars_dt(
  lb,
  dtc = LBDTC,
  new_vars_prefix = "LB",
  highest_imputation = "M"
)

adsl <- adsl %>%
  derive_var_extreme_dt(
    new_var = LSTALVDT,
    ae_start_date, ae_end_date, lb_date, trt_end_date,
    source_datasets = list(ae = ae_ext, adsl = adsl, lb = lb_ext),
    mode = "last"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = vars(USUBJID, TRTEDT, DTHDTC, LSTALVDT),
  filter = !is.na(TRTSDT)
)

## ----eval=TRUE----------------------------------------------------------------
ae_start_date <- date_source(
  dataset_name = "ae",
  date = AESTDT,
  traceability_vars = vars(LALVDOM = "AE", LALVSEQ = AESEQ, LALVVAR = "AESTDTC")
)
ae_end_date <- date_source(
  dataset_name = "ae",
  date = AEENDT,
  traceability_vars = vars(LALVDOM = "AE", LALVSEQ = AESEQ, LALVVAR = "AEENDTC")
)
lb_date <- date_source(
  dataset_name = "lb",
  date = LBDT,
  filter = !is.na(LBDT),
  traceability_vars = vars(LALVDOM = "LB", LALVSEQ = LBSEQ, LALVVAR = "LBDTC")
)
trt_end_date <- date_source(
  dataset_name = "adsl",
  date = TRTEDTM,
  traceability_vars = vars(LALVDOM = "ADSL", LALVSEQ = NA_integer_, LALVVAR = "TRTEDTM")
)

adsl <- adsl %>%
  select(-LSTALVDT) %>% # created in the previous call
  derive_var_extreme_dt(
    new_var = LSTALVDT,
    ae_start_date, ae_end_date, lb_date, trt_end_date,
    source_datasets = list(ae = ae_ext, adsl = adsl, lb = lb_ext),
    mode = "last"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = vars(USUBJID, TRTEDT, DTHDTC, LSTALVDT, LALVDOM, LALVSEQ, LALVVAR),
  filter =  !is.na(TRTSDT)
)

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_var_agegr_fda(
    age_var = AGE,
    new_var = AGEGR1
  )

## ----eval=TRUE----------------------------------------------------------------
format_agegr2 <- function(var_input) {
  case_when(
    var_input < 65 ~ "< 65",
    var_input >= 65 ~ ">= 65",
    TRUE ~ NA_character_
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
    AGEGR2 = format_agegr2(AGE),
    REGION1 = format_region1(COUNTRY)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = vars(USUBJID, AGE, SEX, COUNTRY, AGEGR1, AGEGR2, REGION1)
)

## ----eval=TRUE----------------------------------------------------------------
adsl <- adsl %>%
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = vars(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = vars(USUBJID, TRTSDT, ARM, ACTARM, SAFFL)
)
