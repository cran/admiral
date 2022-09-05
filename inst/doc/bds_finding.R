## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ----message=FALSE------------------------------------------------------------
library(admiral)
library(dplyr)
library(admiral.test)
library(lubridate)
library(stringr)
library(tibble)

data("admiral_adsl")
data("admiral_vs")

adsl <- admiral_adsl
vs <- admiral_vs

vs <- convert_blanks_to_na(vs)

## ----echo=FALSE---------------------------------------------------------------
vs <- filter(vs, USUBJID %in% c("01-701-1015", "01-701-1023", "01-703-1086", "01-703-1096", "01-707-1037", "01-716-1024"))

## ----eval=TRUE----------------------------------------------------------------

adsl_vars <- vars(TRTSDT, TRTEDT, TRT01A, TRT01P)

advs <- derive_vars_merged(
  vs,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = vars(STUDYID, USUBJID)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, VSTESTCD, VSDTC, VISIT, TRTSDT, TRTEDT, TRT01A, TRT01P),
  filter = VSTESTCD == "DIABP" & VISIT == "WEEK 2"
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_vars_dt(advs, new_vars_prefix = "A", dtc = VSDTC)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, VISIT, VSDTC, ADT),
  filter = VSTESTCD == "DIABP"
)

## ----eval=TRUE, include=FALSE-------------------------------------------------
advs_old <- advs

advs <- advs %>%
  mutate(
    VSDTC = if_else(
      USUBJID == "01-716-1024" & VISIT == "SCREENING 1",
      "2012-07",
      VSDTC
    )
  ) %>%
  select(-ADT)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_vars_dt(
  advs,
  new_vars_prefix = "A",
  dtc = VSDTC,
  highest_imputation = "M"
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, VISIT, VSDTC, ADT, ADTF),
  filter = USUBJID == "01-716-1024"
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
advs <- advs_old

## ----eval=FALSE---------------------------------------------------------------
#  # CDISC Pilot data does not contain times and the output of the derivation
#  # ADTM is not presented.
#  advs <- derive_vars_dtm(
#    advs,
#    new_vars_prefix = "A",
#    dtc = VSDTC,
#    highest_imputation = "M"
#  )

## ----eval=TRUE----------------------------------------------------------------
advs <-
  derive_vars_dy(advs, reference_date = TRTSDT, source_vars = vars(ADT))

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, VISIT, ADT, ADY, TRTSDT),
  filter = USUBJID == "01-716-1024"
)

## ----eval=TRUE, include=FALSE-------------------------------------------------
param_lookup <- tribble(
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN, ~PARCAT1, ~PARCAT1N,
  "HEIGHT", "HEIGHT", "Height (cm)", 1, "Subject Characteristic", 1,
  "WEIGHT", "WEIGHT", "Weight (kg)", 2, "Subject Characteristic", 1,
  "DIABP", "DIABP", "Diastolic Blood Pressure (mmHg)", 3, "Vital Sign", 2,
  "MAP", "MAP", "Mean Arterial Pressure (mmHg)", 4, "Vital Sign", 2,
  "BSA", "BSA", "Body Surface Area (m^2)", 5, "Vital Sign", 2,
  "PULSE", "PULSE", "Pulse Rate (beats/min)", 6, "Vital Sign", 2,
  "SYSBP", "SYSBP", "Systolic Blood Pressure (mmHg)", 7, "Vital Sign", 2,
  "TEMP", "TEMP", "Temperature (C)", 8, "Vital Sign", 2
)
attr(param_lookup$VSTESTCD, "label") <- "Vital Signs Test Short Name"

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_vars_merged(
  advs,
  dataset_add = param_lookup,
  new_vars = vars(PARAMCD),
  by_vars = vars(VSTESTCD)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
advs_param <- distinct(advs, USUBJID, PARAMCD, VSTESTCD)

dataset_vignette(advs_param, display_vars = vars(USUBJID, VSTESTCD, PARAMCD))

## ----eval=TRUE----------------------------------------------------------------
advs <- mutate(
  advs,
  AVAL = VSSTRESN,
  AVALC = VSSTRESC
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(VSTESTCD, PARAMCD, VSSTRESN, VSSTRESC, AVAL, AVALC),
  filter = USUBJID == "01-716-1024"
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_param_map(
  advs,
  by_vars = vars(STUDYID, USUBJID, !!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM),
  set_values_to = vars(PARAMCD = "MAP"),
  get_unit_expr = VSSTRESU,
  filter = VSSTAT != "NOT DONE" | is.na(VSSTAT)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  arrange(advs, USUBJID, VISITNUM, VSTPTNUM, ADT, PARAMCD),
  display_vars = vars(VSTESTCD, PARAMCD, VISIT, VSTPT, AVAL, AVALC),
  filter = USUBJID == "01-701-1015" & PARAMCD %in% c("MAP", "DIABP", "SYSBP")
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_param_bsa(
  advs,
  by_vars = vars(STUDYID, USUBJID, !!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM),
  method = "Mosteller",
  set_values_to = vars(PARAMCD = "BSA"),
  get_unit_expr = VSSTRESU,
  filter = VSSTAT != "NOT DONE" | is.na(VSSTAT)
)

advs <- derive_param_bmi(
  advs,
  by_vars = vars(STUDYID, USUBJID, !!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM),
  set_values_to = vars(PARAMCD = "BMI"),
  get_unit_expr = VSSTRESU,
  filter = VSSTAT != "NOT DONE" | is.na(VSSTAT)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  arrange(advs, USUBJID, VISITNUM, VSTPTNUM, ADT, PARAMCD),
  display_vars = vars(USUBJID, VSTESTCD, PARAMCD, VISIT, VSTPT, AVAL, AVALC),
  filter = PARAMCD %in% c("BSA", "BMI")
)

## ----eval=FALSE---------------------------------------------------------------
#  
#  adeg <- tibble::tribble(
#    ~USUBJID, ~EGSTRESU, ~PARAMCD, ~AVAL, ~VISIT,
#    "P01", "msec", "QT", 350, "CYCLE 1 DAY 1",
#    "P01", "msec", "QT", 370, "CYCLE 2 DAY 1",
#    "P01", "msec", "RR", 842, "CYCLE 1 DAY 1",
#    "P01", "msec", "RR", 710, "CYCLE 2 DAY 1"
#  )
#  
#  adeg <- derive_param_qtc(
#    adeg,
#    by_vars = vars(USUBJID, VISIT),
#    method = "Fridericia",
#    set_values_to = vars(PARAMCD = "QTCFR"),
#    get_unit_expr = EGSTRESU
#  )

## ----eval=FALSE---------------------------------------------------------------
#  adlb <- tibble::tribble(
#    ~USUBJID, ~PARAMCD, ~AVAL, ~PARAM, ~VISIT,
#    "P01", "WBC", 33, "Leukocyte Count (10^9/L)", "CYCLE 1 DAY 1",
#    "P01", "WBC", 38, "Leukocyte Count (10^9/L)", "CYCLE 2 DAY 1",
#    "P01", "LYMLE", 0.90, "Lymphocytes (fraction of 1)", "CYCLE 1 DAY 1",
#    "P01", "LYMLE", 0.70, "Lymphocytes (fraction of 1)", "CYCLE 2 DAY 1"
#  )
#  
#  derive_param_wbc_abs(
#    dataset = adlb,
#    by_vars = vars(USUBJID, VISIT),
#    set_values_to = vars(
#      PARAMCD = "LYMPH",
#      PARAM = "Lymphocytes Abs (10^9/L)",
#      DTYPE = "CALCULATION"
#    ),
#    get_unit_expr = extract_unit(PARAM),
#    wbc_code = "WBC",
#    diff_code = "LYMLE",
#    diff_type = "fraction"
#  )

## ----eval=TRUE----------------------------------------------------------------

# Derive PARAM and PARAMN
advs <- derive_vars_merged(
  advs,
  dataset_add = select(param_lookup, -VSTESTCD),
  by_vars = vars(PARAMCD)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(VSTESTCD, PARAMCD, PARAM, PARAMN, PARCAT1, PARCAT1N),
  filter = USUBJID == "01-716-1024"
)

## ----eval=TRUE----------------------------------------------------------------
advs <- advs %>%
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN") ~ NA_character_,
      str_detect(VISIT, "UNSCHED") ~ NA_character_,
      str_detect(VISIT, "RETRIEVAL") ~ NA_character_,
      str_detect(VISIT, "AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ str_to_title(VISIT)
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "BASELINE" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", ""))
    )),
    ATPT = VSTPT,
    ATPTN = VSTPTNUM
  )


count(advs, VISITNUM, VISIT, AVISITN, AVISIT)

count(advs, VSTPTNUM, VSTPT, ATPTN, ATPT)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_ontrtfl(
  advs,
  start_date = ADT,
  ref_start_date = TRTSDT,
  ref_end_date = TRTEDT
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, PARAMCD, ADT, TRTSDT, TRTEDT, ONTRTFL),
  filter = PARAMCD == "DIABP" & VISIT == "WEEK 2"
)

## ----include=FALSE------------------------------------------------------------
advs <- select(advs, -ONTRTFL)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_ontrtfl(
  advs,
  start_date = ADT,
  ref_start_date = TRTSDT,
  ref_end_date = TRTEDT,
  ref_end_window = 60
)

## ----include=FALSE------------------------------------------------------------
advs <- select(advs, -ONTRTFL)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_ontrtfl(
  advs,
  start_date = ADT,
  ref_start_date = TRTSDT,
  ref_end_date = TRTEDT,
  filter_pre_timepoint = ATPT == "AFTER LYING DOWN FOR 5 MINUTES"
)

## ----include=FALSE------------------------------------------------------------
advs_pre <- select(advs, -ONTRTFL)

advs <- tibble::tribble(
  ~USUBJID, ~ASTDT, ~AP01SDT, ~AP01EDT, ~AENDT,
  "P01", ymd("2020-03-15"), ymd("2020-01-01"), ymd("2020-03-01"), ymd("2020-12-01"),
  "P02", ymd("2019-04-30"), ymd("2020-01-01"), ymd("2020-03-01"), ymd("2020-03-15"),
  "P03", ymd("2019-04-30"), ymd("2020-01-01"), ymd("2020-03-01"), NA,
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_ontrtfl(
  advs,
  new_var = ONTR01FL,
  start_date = ASTDT,
  end_date = AENDT,
  ref_start_date = AP01SDT,
  ref_end_date = AP01EDT,
  span_period = "Y"
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, ASTDT, AENDT, AP01SDT, AP01EDT, ONTR01FL)
)

## ----include=FALSE------------------------------------------------------------
range_lookup <- tibble::tribble(
  ~PARAMCD, ~ANRLO, ~ANRHI, ~A1LO, ~A1HI,
  "SYSBP",      90,    130,    70,   140,
  "DIABP",      60,     80,    40,    90,
  "PULSE",      60,    100,    40,   110,
  "TEMP",     36.5,   37.5,    35,    38
)

advs <- derive_vars_merged(
  advs_pre,
  dataset_add = range_lookup,
  by_vars = vars(PARAMCD)
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_anrind(advs)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, PARAMCD, AVAL, ANRLO, ANRHI, A1LO, A1HI, ANRIND),
  filter = PARAMCD == "DIABP" & VISIT == "WEEK 2"
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_basetype(
  dataset = advs,
  basetypes = rlang::exprs(
    "LAST: AFTER LYING DOWN FOR 5 MINUTES" = ATPTN == 815,
    "LAST: AFTER STANDING FOR 1 MINUTE" = ATPTN == 816,
    "LAST: AFTER STANDING FOR 3 MINUTES" = ATPTN == 817,
    "LAST" = is.na(ATPTN)
  )
)

count(advs, ATPT, ATPTN, BASETYPE)

## ----eval=TRUE----------------------------------------------------------------
advs <- restrict_derivation(
  advs,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(STUDYID, USUBJID, BASETYPE, PARAMCD),
    order = vars(ADT, ATPTN, VISITNUM),
    new_var = ABLFL,
    mode = "last"
  ),
  filter = (!is.na(AVAL) & ADT <= TRTSDT & !is.na(BASETYPE))
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, BASETYPE, PARAMCD, ADT, TRTSDT, ATPTN, TRTSDT, ABLFL),
  filter = PARAMCD == "DIABP" & VISIT %in% c("WEEK 2", "BASELINE")
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_base(
  advs,
  by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
  source_var = AVAL,
  new_var = BASE
)

advs <- derive_var_base(
  advs,
  by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
  source_var = AVALC,
  new_var = BASEC
)

advs <- derive_var_base(
  advs,
  by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
  source_var = ANRIND,
  new_var = BNRIND
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, BASETYPE, PARAMCD, ABLFL, BASE, BASEC, ANRIND, BNRIND),
  filter = PARAMCD == "DIABP" & VISIT %in% c("WEEK 2", "BASELINE")
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_chg(advs)

advs <- derive_var_pchg(advs)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, VISIT, BASE, AVAL, CHG, PCHG),
  filter = PARAMCD == "DIABP" & VISIT %in% c("WEEK 2", "WEEK 8")
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_shift(advs,
  new_var = SHIFT1,
  from_var = BNRIND,
  to_var = ANRIND
)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_analysis_ratio(advs,
  numer_var = AVAL,
  denom_var = BASE
)

advs <- derive_var_analysis_ratio(advs,
  numer_var = AVAL,
  denom_var = ANRLO,
  new_var = R01ANRLO
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, VISIT, BASE, AVAL, ANRLO, R2BASE, R01ANRLO),
  filter = PARAMCD == "DIABP" & VISIT %in% c("WEEK 2", "WEEK 8")
)

## ----eval=TRUE----------------------------------------------------------------

advs <- restrict_derivation(
  advs,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(STUDYID, USUBJID, BASETYPE, PARAMCD, AVISIT),
    order = vars(ADT, ATPTN, AVAL),
    new_var = ANL01FL,
    mode = "last"
  ),
  filter = !is.na(AVISITN)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, PARAMCD, AVISIT, ATPTN, ADT, AVAL, ANL01FL),
  filter = PARAMCD == "DIABP" & VISIT %in% c("WEEK 2", "WEEK 8")
)

## ----eval=TRUE----------------------------------------------------------------

advs <- restrict_derivation(
  advs,
  derivation = derive_var_worst_flag,
  args = params(
    by_vars = vars(STUDYID, USUBJID, BASETYPE, PARAMCD, AVISIT),
    order = vars(ADT, ATPTN),
    new_var = WORSTFL,
    param_var = PARAMCD,
    analysis_var = AVAL,
    worst_high = c("SYSBP", "DIABP"),
    worst_low = "PULSE"
  ),
  filter = !is.na(AVISIT) & !is.na(AVAL)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, PARAMCD, AVISIT, AVAL, ADT, ATPTN, WORSTFL),
  filter = USUBJID == "01-701-1015"
)

## ----eval=TRUE----------------------------------------------------------------
advs <- mutate(advs, TRTP = TRT01P, TRTA = TRT01A)

count(advs, TRTP, TRTA, TRT01P, TRT01A)

## ----eval=TRUE----------------------------------------------------------------
advs <- derive_var_obs_number(
  advs,
  new_var = ASEQ,
  by_vars = vars(STUDYID, USUBJID),
  order = vars(PARAMCD, ADT, AVISITN, VISITNUM, ATPTN),
  check_type = "error"
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, PARAMCD, ADT, AVISITN, ATPTN, VISIT, ADT, ASEQ),
  filter = USUBJID == "01-701-1015"
)

## ----eval=TRUE----------------------------------------------------------------
avalcat_lookup <- tibble::tribble(
  ~PARAMCD, ~AVALCA1N, ~AVALCAT1,
  "HEIGHT", 1, ">140 cm",
  "HEIGHT", 2, "<= 140 cm"
)

format_avalcat1n <- function(param, aval) {
  case_when(
    param == "HEIGHT" & aval > 140 ~ 1,
    param == "HEIGHT" & aval <= 140 ~ 2
  )
}

advs <- advs %>%
  mutate(AVALCA1N = format_avalcat1n(param = PARAMCD, aval = AVAL)) %>%
  derive_vars_merged(
    avalcat_lookup,
    by = vars(PARAMCD, AVALCA1N)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, PARAMCD, AVAL, AVALCA1N, AVALCAT1),
  filter = PARAMCD == "HEIGHT"
)

## ----eval=TRUE----------------------------------------------------------------
advs <- advs %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = vars(STUDYID, USUBJID)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  advs,
  display_vars = vars(USUBJID, RFSTDTC, RFENDTC, DTHDTC, DTHFL, AGE, AGEU),
  filter = USUBJID == "01-701-1015"
)

## ----eval=TRUE----------------------------------------------------------------
advs_ex1 <- advs %>%
  derive_extreme_records(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    order = vars(ADT, AVISITN, ATPTN, AVAL),
    mode = "last",
    filter = (4 < AVISITN & AVISITN <= 12 & ANL01FL == "Y"),
    set_values_to = vars(
      AVISIT = "End of Treatment",
      AVISITN = 99,
      DTYPE = "LOV"
    )
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  arrange(advs_ex1, USUBJID, PARAMCD, desc(AVISITN), ATPTN),
  display_vars = vars(USUBJID, PARAMCD, ADT, AVISITN, AVISIT, ATPTN, AVAL, DTYPE, ANL01FL),
  filter = USUBJID == "01-701-1015" & ANL01FL == "Y"
)

## ----eval=TRUE----------------------------------------------------------------
advs_ex1 <- advs %>%
  derive_extreme_records(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    order = vars(AVAL, ADT, AVISITN, ATPTN),
    mode = "first",
    filter = (4 < AVISITN & AVISITN <= 12 & ANL01FL == "Y" & !is.na(AVAL)),
    set_values_to = vars(
      AVISIT = "Minimum on Treatment",
      AVISITN = 98,
      DTYPE = "MINIMUM"
    )
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  arrange(advs_ex1, USUBJID, PARAMCD, desc(AVISITN), ATPTN),
  display_vars = vars(USUBJID, PARAMCD, ADT, AVISITN, AVISIT, ATPTN, AVAL, DTYPE, ANL01FL),
  filter = USUBJID == "01-701-1015" & ANL01FL == "Y"
)

## ----eval=TRUE----------------------------------------------------------------
advs_ex2 <- derive_summary_records(
  advs,
  by_vars = vars(STUDYID, USUBJID, PARAMCD, VISITNUM, ADT),
  analysis_var = AVAL,
  summary_fun = mean,
  set_values_to = vars(DTYPE = "AVERAGE")
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  arrange(advs_ex2, USUBJID, PARAMCD, VISITNUM, ADT, DTYPE),
  display_vars = vars(USUBJID, PARAMCD, VISITNUM, ADT, AVAL, DTYPE),
  filter = USUBJID == "01-701-1015"
)

## ----eval=TRUE----------------------------------------------------------------
advs_ex3 <- derive_param_computed(
  advs,
  by_vars = vars(USUBJID, VISIT, ATPT),
  parameters = c("SYSBP", "DIABP"),
  analysis_value = (AVAL.SYSBP - AVAL.DIABP) / 3 + AVAL.DIABP,
  set_values_to = vars(
    PARAMCD = "MAP2",
    PARAM = "Mean Arterial Pressure 2 (mmHg)"
  )
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  arrange(advs_ex3, USUBJID, VISIT, ATPT, PARAMCD),
  display_vars = vars(USUBJID, PARAMCD, VISIT, ATPT, AVAL),
  filter = USUBJID == "01-701-1015" & PARAMCD %in% c("MAP2", "SYSBP", "DIABP")
)

