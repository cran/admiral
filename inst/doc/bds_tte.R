## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(admiral)
library(dplyr)
library(admiral.test)

## ---- warning=FALSE, message=FALSE, include=FALSE-----------------------------
library(lubridate)

## -----------------------------------------------------------------------------
data("adsl")
data("adae")

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(admiral:::list_tte_source_objects())

## -----------------------------------------------------------------------------
adtte <- derive_param_tte(
  dataset_adsl = adsl,
  start_date = TRTSDT,
  event_conditions = list(ae_ser_event),
  censor_conditions = list(lastalive_censor),
  source_datasets = list(adsl = adsl, adae = adae),
  set_values_to = vars(PARAMCD = "TTAESER", PARAM = "Time to First Serious AE")
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(
  adtte,
  display_vars = vars(USUBJID, PARAMCD, PARAM, STARTDT, ADT, CNSR)
)

## -----------------------------------------------------------------------------
death <- event_source(
  dataset_name = "adsl",
  filter = DTHFL == "Y",
  date = DTHDT
)

## -----------------------------------------------------------------------------
lstalv <- censor_source(
  dataset_name = "adsl",
  date = LSTALVDT
)

## -----------------------------------------------------------------------------
adtte <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adsl = adsl),
  start_date = TRTSDT,
  event_conditions = list(death),
  censor_conditions = list(lstalv),
  set_values_to = vars(PARAMCD = "OS", PARAM = "Overall Survival")
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(
  adtte,
  display_vars = vars(USUBJID, PARAMCD, PARAM, STARTDT, ADT, CNSR)
)

## -----------------------------------------------------------------------------
# define death event #
death <- event_source(
  dataset_name = "adsl",
  filter = DTHFL == "Y",
  date = DTHDT,
  set_values_to =vars(
    EVNTDESC = "DEATH",
    SRCDOM = "ADSL",
    SRCVAR = "DTHDT"
  )
)

# define censoring at last known alive date #
lstalv <- censor_source(
  dataset_name = "adsl",
  date = LSTALVDT,
  set_values_to = vars(
    EVNTDESC = "LAST KNOWN ALIVE DATE",
    SRCDOM = "ADSL",
    SRCVAR = "LSTALVDT"
  )
)

# derive time-to-event parameter #
adtte <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adsl = adsl),
  event_conditions = list(death),
  censor_conditions = list(lstalv),
  set_values_to = vars(PARAMCD = "OS", PARAM = "Overall Survival")
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(
  adtte,
  display_vars = vars(USUBJID, EVNTDESC, SRCDOM, SRCVAR, CNSR, ADT)
)
# save adtte and adsl for next section
adtte_bak <- adtte
adsl_bak <- adsl

## ---- echo=FALSE--------------------------------------------------------------
adsl <- tibble::tribble(
    ~USUBJID, ~DTHFL, ~DTHDT,            ~TRTSDT,           ~TRTSDTF,
    "01",     "Y",    ymd("2021-06-12"), ymd("2021-01-01"), "M",
    "02",     "N",    NA,                ymd("2021-02-03"), NA,
    "03",     "Y",    ymd("2021-08-21"), ymd("2021-08-10"), NA,
    "04",     "N",    NA,                ymd("2021-02-03"), NA,
    "05",     "N",    NA,                ymd("2021-04-01"), "D"
  ) %>%
    mutate(STUDYID = "AB42")

dataset_vignette(
  adsl,
  display_vars = vars(USUBJID, DTHFL, DTHDT, TRTSDT, TRTSDTF)
)

## ---- echo=FALSE--------------------------------------------------------------
  adrs <- tibble::tribble(
    ~USUBJID, ~AVALC, ~ADT,              ~ASEQ,
    "01",     "SD",   ymd("2021-01-03"), 1,
    "01",     "PR",   ymd("2021-03-04"), 2,
    "01",     "PD",   ymd("2021-05-05"), 3,
    "02",     "PD",   ymd("2021-02-03"), 1,
    "04",     "SD",   ymd("2021-02-13"), 1,
    "04",     "PR",   ymd("2021-04-14"), 2,
    "04",     "CR",   ymd("2021-05-15"), 3
  ) %>%
    mutate(
      STUDYID = "AB42",
      PARAMCD = "OVR",
      PARAM = "Overall Response"
    ) %>% 
    select(STUDYID, USUBJID, PARAMCD, PARAM, ADT, ASEQ, AVALC)

dataset_vignette(
  adrs,
  display_vars = vars(USUBJID, AVALC, ADT, ASEQ, PARAMCD, PARAM)
)

## -----------------------------------------------------------------------------
# progressive disease event #
pd <- event_source(
  dataset_name = "adrs",
  filter = AVALC == "PD",
  date = ADT,
  set_values_to = vars(
    EVNTDESC = "PD",
    SRCDOM = "ADRS",
    SRCVAR = "ADT",
    SRCSEQ = ASEQ
  )
)

# death event #
death <- event_source(
  dataset_name = "adsl",
  filter = DTHFL == "Y",
  date = DTHDT,
  set_values_to = vars(
    EVNTDESC = "DEATH",
    SRCDOM = "ADSL",
    SRCVAR = "DTHDT"
  )
)

## -----------------------------------------------------------------------------
# last tumor assessment censoring (CNSR = 1 by default) #
lastvisit <- censor_source(
  dataset_name = "adrs",
  date = ADT,
  set_values_to = vars(
    EVNTDESC = "LAST TUMOR ASSESSMENT",
    SRCDOM = "ADRS",
    SRCVAR = "ADT"
  )
)

## -----------------------------------------------------------------------------
# start date censoring (for patients without tumor assessment) (CNSR = 2) #
start <- censor_source(
  dataset_name = "adsl",
  date = TRTSDT,
  censor = 2,
  set_values_to = vars(
    EVNTDESC = "TREATMENT START",
    SRCDOM = "ADSL",
    SRCVAR = "TRTSDT",
    ADTF = TRTSDTF
  )
)

# derive time-to-event parameter #
adtte <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adsl = adsl, adrs = adrs),
  start_date = TRTSDT,
  start_date_imputation_flag = TRTSDTF,
  event_conditions = list(pd, death),
  censor_conditions = list(lastvisit, start),
  set_values_to = vars(PARAMCD = "PFS", PARAM = "Progression Free Survival")
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(
  adtte %>%
  select(
    STUDYID, USUBJID, PARAMCD, PARAM, STARTDT, STARTDTF, ADT, ADTF, CNSR,
    EVNTDESC, SRCDOM, SRCVAR
  ),
  display_vars = vars(USUBJID, PARAMCD, STARTDT, STARTDTF, ADT, ADTF, CNSR)
)

## ----echo=FALSE---------------------------------------------------------------
adtte <- adtte_bak
adsl <- adsl_bak

## -----------------------------------------------------------------------------
# define censoring #
observation_end <- censor_source(
  dataset_name = "adsl",
  date = EOSDT,
  censor = 1,
  set_values_to = vars(
    EVNTDESC = "END OF STUDY",
    SRCDOM = "ADSL",
    SRCVAR = "EOSDT"
  )
)

# define time to first AE #
tt_ae <- event_source(
  dataset_name = "ae",
  date = AESTDTC,
  set_values_to = vars(
    EVNTDESC = "ADVERSE EVENT",
    SRCDOM = "AE",
    SRCVAR = "AESTDTC"
  )
)

# define time to first serious AE #
tt_ser_ae <- event_source(
  dataset_name = "ae",
  filter = AESER == "Y",
  date = AESTDTC,
  set_values_to = vars(
    EVNTDESC = "SERIOUS ADVERSE EVENT",
    SRCDOM = "AE",
    SRCVAR = "AESTDTC"
  )
)

# define time to first related AE #
tt_rel_ae <- event_source(
  dataset_name = "ae",
  filter = AEREL %in% c("PROBABLE", "POSSIBLE", "REMOTE"),
  date = AESTDTC,
  set_values_to = vars(
    EVNTDESC = "RELATED ADVERSE EVENT",
    SRCDOM = "AE",
    SRCVAR = "AESTDTC"
  )
)

# derive all three time to event parameters #
adaette <- call_derivation(
  derivation = derive_param_tte,
  variable_params = list(
    params(
      event_conditions = list(tt_ae),
      set_values_to = vars(PARAMCD = "TTAE")
    ),
    params(
      event_conditions = list(tt_ser_ae),
      set_values_to = vars(PARAMCD = "TTSERAE")
    ),
    params(
      event_conditions = list(tt_rel_ae),
      set_values_to = vars(PARAMCD = "TTRELAE")
    )
  ),
  dataset_adsl = adsl,
  source_datasets = list(adsl = adsl, ae = ae),
  censor_conditions = list(observation_end)
)

## ---- echo=FALSE--------------------------------------------------------------
adaette %>%
  select(STUDYID, USUBJID, PARAMCD, STARTDT, ADT, CNSR, EVNTDESC, SRCDOM, SRCVAR) %>%
  arrange(USUBJID, PARAMCD) %>% 
  dataset_vignette(display_vars = vars(USUBJID, PARAMCD, STARTDT, ADT, CNSR, EVNTDESC, SRCDOM, SRCVAR))

## ---- echo=FALSE--------------------------------------------------------------
adsl <- tibble::tribble(
  ~USUBJID, ~TRTSDT,           ~EOSDT,
  "01",     ymd("2020-12-06"), ymd("2021-03-06"),
  "02",     ymd("2021-01-16"), ymd("2021-02-03")
) %>%
  mutate(STUDYID = "AB42")

dataset_vignette(adsl)

## ---- echo=FALSE--------------------------------------------------------------
ae <- tibble::tribble(
  ~USUBJID, ~AESTDTC,           ~AESEQ, ~AEDECOD,
  "01",     "2021-01-03T10:56", 1,      "Flu",
  "01",     "2021-03-04",       2,      "Cough",
  "01",     "2021",             3,      "Flu"
) %>%
  mutate(STUDYID = "AB42")

dataset_vignette(ae)

## -----------------------------------------------------------------------------
# define time to first adverse event event #
ttae <- event_source(
  dataset_name = "ae",
  date = AESTDTC,
  set_values_to = vars(
    EVNTDESC = "AE",
    SRCDOM = "AE",
    SRCVAR = "AESTDTC",
    SRCSEQ = AESEQ
  )
)

# define censoring at end of study #
eos <- censor_source(
  dataset_name = "adsl",
  date = EOSDT,
  set_values_to = vars(
    EVNTDESC = "END OF STUDY",
    SRCDOM = "ADSL",
    SRCVAR = "EOSDT"
  )
)

# derive time-to-event parameter #
adtte <- derive_param_tte(
  dataset_adsl = adsl,
  by_vars = vars(AEDECOD),
  start_date = TRTSDT,
  event_conditions = list(ttae),
  censor_conditions = list(eos),
  source_datasets = list(adsl = adsl, ae = ae),
  set_values_to = vars(
    PARAMCD = paste0("TTAE", as.numeric(as.factor(AEDECOD))),
    PARAM = paste("Time to First", AEDECOD, "Adverse Event"),
    PARCAT1 = "TTAE",
    PARCAT2 = AEDECOD
  )
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(
  adtte %>%
    select(
      USUBJID, STARTDT, PARAMCD, PARAM, PARCAT1, PARCAT2, ADT, CNSR,
      EVNTDESC, SRCDOM, SRCVAR, SRCSEQ
    ),
  display_vars = vars(USUBJID, STARTDT, PARAMCD, PARAM, ADT, CNSR, SRCSEQ)
)

## ----echo=FALSE---------------------------------------------------------------
adtte <- adtte_bak
adsl <- adsl_bak

## ----eval=TRUE----------------------------------------------------------------
adtte <- derive_vars_duration(
  adtte,
  new_var = AVAL,
  start_date = STARTDT,
  end_date = ADT
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(
  adtte)

## ----eval=TRUE----------------------------------------------------------------
adtte <- derive_var_obs_number(
  adtte,
  by_vars = vars(STUDYID, USUBJID),
  order = vars(PARAMCD),
  check_type = "error"
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(adtte)

## ----eval=TRUE----------------------------------------------------------------
adtte <- left_join(
  adtte,
  select(adsl, STUDYID, USUBJID, ARMCD, ARM, ACTARMCD, ACTARM, AGE, SEX),
  by = c("STUDYID", "USUBJID")
)

## ---- echo=FALSE--------------------------------------------------------------
dataset_vignette(adtte)

