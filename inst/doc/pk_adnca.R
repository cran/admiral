## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ----message=FALSE------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(admiral)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(tibble)

ex <- pharmaversesdtm::ex
pc <- pharmaversesdtm::pc
vs <- pharmaversesdtm::vs
lb <- pharmaversesdtm::lb
adsl <- admiral::admiral_adsl

ex <- convert_blanks_to_na(ex)
pc <- convert_blanks_to_na(pc)
vs <- convert_blanks_to_na(vs)
lb <- convert_blanks_to_na(lb) %>%
  filter(LBBLFL == "Y")

# ---- Lookup tables ----
param_lookup <- tibble::tribble(
  ~PCTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "XAN", "XAN", "Pharmacokinetic concentration of Xanomeline", 1,
  "DOSE", "DOSE", "Xanomeline Patch Dose", 2,
)

## ----echo=FALSE---------------------------------------------------------------
ex <- filter(ex, USUBJID %in% c(
  "01-701-1028", "01-701-1033", "01-701-1442", "01-714-1288", "01-718-1101"
))
pc <- filter(pc, USUBJID %in% c(
  "01-701-1028", "01-701-1033", "01-701-1442", "01-714-1288", "01-718-1101"
))

## ----eval=TRUE----------------------------------------------------------------
adsl_vars <- exprs(TRTSDT, TRTSDTM, TRT01P, TRT01A)

pc_dates <- pc %>%
  # Join ADSL with PC (need TRTSDT for ADY derivation)
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  # Derive analysis date/time
  # Impute missing time to 00:00:00
  derive_vars_dtm(
    new_vars_prefix = "A",
    dtc = PCDTC,
    time_imputation = "00:00:00"
  ) %>%
  # Derive dates and times from date/times
  derive_vars_dtm_to_dt(exprs(ADTM)) %>%
  derive_vars_dtm_to_tm(exprs(ADTM)) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT)) %>%
  # Derive event ID and nominal relative time from first dose (NFRLT)
  mutate(
    EVID = 0,
    DRUG = PCTEST,
    NFRLT = if_else(PCTPTNUM < 0, 0, PCTPTNUM), .after = USUBJID
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  pc_dates,
  display_vars = exprs(
    USUBJID, PCTEST, ADTM, VISIT, PCTPT, NFRLT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Get dosing information ----

ex_dates <- ex %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  # Keep records with nonzero dose
  filter(EXDOSE > 0) %>%
  # Add time and set missing end date to start date
  # Impute missing time to 00:00:00
  # Note all times are missing for dosing records in this example data
  # Derive Analysis Start and End Dates
  derive_vars_dtm(
    new_vars_prefix = "AST",
    dtc = EXSTDTC,
    time_imputation = "00:00:00"
  ) %>%
  derive_vars_dtm(
    new_vars_prefix = "AEN",
    dtc = EXENDTC,
    time_imputation = "00:00:00"
  ) %>%
  # Derive event ID and nominal relative time from first dose (NFRLT)
  mutate(
    EVID = 1,
    NFRLT = case_when(
      VISITDY == 1 ~ 0,
      TRUE ~ 24 * VISITDY
    )
  ) %>%
  # Set missing end dates to start date
  mutate(AENDTM = case_when(
    is.na(AENDTM) ~ ASTDTM,
    TRUE ~ AENDTM
  )) %>%
  # Derive dates from date/times
  derive_vars_dtm_to_dt(exprs(ASTDTM)) %>%
  derive_vars_dtm_to_dt(exprs(AENDTM))

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  ex_dates,
  display_vars = exprs(
    USUBJID, EXTRT, EXDOSFRQ, ASTDTM, AENDTM, VISIT, VISITDY, NFRLT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Expand dosing records between start and end dates ----

ex_exp <- ex_dates %>%
  create_single_dose_dataset(
    dose_freq = EXDOSFRQ,
    start_date = ASTDT,
    start_datetime = ASTDTM,
    end_date = AENDT,
    end_datetime = AENDTM,
    nominal_time = NFRLT,
    lookup_table = dose_freq_lookup,
    lookup_column = CDISC_VALUE,
    keep_source_vars = exprs(
      STUDYID, USUBJID, EVID, EXDOSFRQ, EXDOSFRM,
      NFRLT, EXDOSE, EXDOSU, EXTRT, ASTDT, ASTDTM, AENDT, AENDTM,
      VISIT, VISITNUM, VISITDY, TRT01A, TRT01P, DOMAIN, EXSEQ, !!!adsl_vars
    )
  ) %>%
  # Derive AVISIT based on nominal relative time
  # Derive AVISITN to nominal time in whole days using integer division
  # Define AVISIT based on nominal day
  mutate(
    AVISITN = NFRLT %/% 24 + 1,
    AVISIT = paste("Day", AVISITN),
    ADTM = ASTDTM,
    DRUG = EXTRT,
  ) %>%
  # Derive dates and times from datetimes
  derive_vars_dtm_to_dt(exprs(ADTM)) %>%
  derive_vars_dtm_to_tm(exprs(ADTM)) %>%
  derive_vars_dtm_to_tm(exprs(ASTDTM)) %>%
  derive_vars_dtm_to_tm(exprs(AENDTM)) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  ex_exp,
  display_vars = exprs(
    USUBJID, DRUG, EXDOSFRQ, ASTDTM, AENDTM, AVISIT, NFRLT
  )
)

## ----eval=TRUE, echo=TRUE, message=FALSE--------------------------------------
# ---- Find first dose per treatment per subject ----
# ---- Join with ADPC data and keep only subjects with dosing ----

adpc_first_dose <- pc_dates %>%
  derive_vars_merged(
    dataset_add = ex_exp,
    filter_add = (EXDOSE > 0 & !is.na(ADTM)),
    new_vars = exprs(FANLDTM = ADTM),
    order = exprs(ADTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID, DRUG)
  ) %>%
  filter(!is.na(FANLDTM)) %>%
  # Derive AVISIT based on nominal relative time
  # Derive AVISITN to nominal time in whole days using integer division
  # Define AVISIT based on nominal day
  mutate(
    AVISITN = NFRLT %/% 24 + 1,
    AVISIT = paste("Day", AVISITN)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adpc_first_dose,
  display_vars = exprs(
    USUBJID, FANLDTM, AVISIT, ADTM, PCTPT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Find previous dose  ----

adpc_prev <- adpc_first_dose %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(ADTM),
    new_vars = exprs(
      ADTM_prev = ADTM, EXDOSE_prev = EXDOSE, AVISIT_prev = AVISIT,
      AENDTM_prev = AENDTM
    ),
    join_vars = exprs(ADTM),
    join_type = "all",
    filter_add = NULL,
    filter_join = ADTM > ADTM.join,
    mode = "last",
    check_type = "none"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adpc_prev,
  display_vars = exprs(
    USUBJID, VISIT, ADTM, VISIT, PCTPT, ADTM_prev, EXDOSE_prev, AVISIT_prev
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Find next dose  ----

adpc_next <- adpc_prev %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(ADTM),
    new_vars = exprs(
      ADTM_next = ADTM, EXDOSE_next = EXDOSE, AVISIT_next = AVISIT,
      AENDTM_next = AENDTM
    ),
    join_vars = exprs(ADTM),
    join_type = "all",
    filter_add = NULL,
    filter_join = ADTM <= ADTM.join,
    mode = "first",
    check_type = "none"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adpc_next,
  display_vars = exprs(
    USUBJID,
    VISIT, ADTM, VISIT, PCTPT, ADTM_next, EXDOSE_next, AVISIT_next
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Find previous nominal time ----

adpc_nom_prev <- adpc_next %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(NFRLT),
    new_vars = exprs(NFRLT_prev = NFRLT),
    join_vars = exprs(NFRLT),
    join_type = "all",
    filter_add = NULL,
    filter_join = NFRLT > NFRLT.join,
    mode = "last",
    check_type = "none"
  )

# ---- Find next nominal time ----

adpc_nom_next <- adpc_nom_prev %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(NFRLT),
    new_vars = exprs(NFRLT_next = NFRLT),
    join_vars = exprs(NFRLT),
    join_type = "all",
    filter_add = NULL,
    filter_join = NFRLT <= NFRLT.join,
    mode = "first",
    check_type = "none"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adpc_nom_next,
  display_vars = exprs(
    USUBJID, NFRLT, PCTPT, NFRLT_prev, NFRLT_next
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Combine ADPC and EX data ----
# Derive Relative Time Variables

adpc_arrlt <- bind_rows(adpc_nom_next, ex_exp) %>%
  group_by(USUBJID, DRUG) %>%
  mutate(
    FANLDTM = min(FANLDTM, na.rm = TRUE),
    min_NFRLT = min(NFRLT_prev, na.rm = TRUE),
    maxdate = max(ADT[EVID == 0], na.rm = TRUE), .after = USUBJID
  ) %>%
  arrange(USUBJID, ADTM) %>%
  ungroup() %>%
  filter(ADT <= maxdate) %>%
  # Derive Actual Relative Time from First Dose (AFRLT)
  derive_vars_duration(
    new_var = AFRLT,
    start_date = FANLDTM,
    end_date = ADTM,
    out_unit = "hours",
    floor_in = FALSE,
    add_one = FALSE
  ) %>%
  # Derive Actual Relative Time from Reference Dose (ARRLT)
  derive_vars_duration(
    new_var = ARRLT,
    start_date = ADTM_prev,
    end_date = ADTM,
    out_unit = "hours",
    floor_in = FALSE,
    add_one = FALSE
  ) %>%
  # Derive Actual Relative Time from Next Dose (AXRLT not kept)
  derive_vars_duration(
    new_var = AXRLT,
    start_date = ADTM_next,
    end_date = ADTM,
    out_unit = "hours",
    floor_in = FALSE,
    add_one = FALSE
  ) %>%
  mutate(
    ARRLT = case_when(
      EVID == 1 ~ 0,
      is.na(ARRLT) ~ AXRLT,
      TRUE ~ ARRLT
    ),

    # Derive Reference Dose Date
    PCRFTDTM = case_when(
      EVID == 1 ~ ADTM,
      is.na(ADTM_prev) ~ ADTM_next,
      TRUE ~ ADTM_prev
    )
  ) %>%
  # Derive dates and times from datetimes
  derive_vars_dtm_to_dt(exprs(FANLDTM)) %>%
  derive_vars_dtm_to_tm(exprs(FANLDTM)) %>%
  derive_vars_dtm_to_dt(exprs(PCRFTDTM)) %>%
  derive_vars_dtm_to_tm(exprs(PCRFTDTM))

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adpc_arrlt,
  display_vars = exprs(
    USUBJID, FANLDTM, AVISIT, PCTPT, AFRLT, ARRLT, AXRLT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adpc_nrrlt <- adpc_arrlt %>%
  # Derive Nominal Relative Time from Reference Dose (NRRLT)
  mutate(
    NRRLT = case_when(
      EVID == 1 ~ 0,
      is.na(NFRLT_prev) ~ NFRLT - min_NFRLT,
      TRUE ~ NFRLT - NFRLT_prev
    ),
    NXRLT = case_when(
      EVID == 1 ~ 0,
      TRUE ~ NFRLT - NFRLT_next
    )
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adpc_nrrlt,
  display_vars = exprs(
    USUBJID, AVISIT, PCTPT, NFRLT, NRRLT, NXRLT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Derive Analysis Variables ----
# Derive ATPTN, ATPT, ATPTREF, and BASETYPE
# Derive planned dose DOSEP, actual dose DOSEA and units
# Derive PARAMCD and relative time units
# Derive AVAL, AVALU and AVALCAT1

adpc_aval <- adpc_nrrlt %>%
  mutate(
    PARCAT1 = PCSPEC,
    ATPTN = case_when(
      EVID == 1 ~ 0,
      TRUE ~ PCTPTNUM
    ),
    ATPT = case_when(
      EVID == 1 ~ "Dose",
      TRUE ~ PCTPT
    ),
    ATPTREF = case_when(
      EVID == 1 ~ AVISIT,
      is.na(AVISIT_prev) ~ AVISIT_next,
      TRUE ~ AVISIT_prev
    ),
    # Derive BASETYPE
    BASETYPE = paste(ATPTREF, "Baseline"),

    # Derive Actual Dose
    DOSEA = case_when(
      EVID == 1 ~ EXDOSE,
      is.na(EXDOSE_prev) ~ EXDOSE_next,
      TRUE ~ EXDOSE_prev
    ),
    # Derive Planned Dose
    DOSEP = case_when(
      TRT01P == "Xanomeline High Dose" ~ 81,
      TRT01P == "Xanomeline Low Dose" ~ 54
    ),
    DOSEU = "mg",
  ) %>%
  # Derive relative time units
  mutate(
    FRLTU = "h",
    RRLTU = "h",
    # Derive PARAMCD
    PARAMCD = coalesce(PCTESTCD, "DOSE"),
    ALLOQ = PCLLOQ,
    # Derive AVAL
    AVAL = case_when(
      EVID == 1 ~ EXDOSE,
      TRUE ~ PCSTRESN
    ),
    AVALU = case_when(
      EVID == 1 ~ EXDOSU,
      TRUE ~ PCSTRESU
    ),
    AVALCAT1 = if_else(PCSTRESC == "<BLQ", PCSTRESC, prettyNum(signif(AVAL, digits = 3))),
  ) %>%
  # Add SRCSEQ
  mutate(
    SRCDOM = DOMAIN,
    SRCVAR = "SEQ",
    SRCSEQ = coalesce(PCSEQ, EXSEQ)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adpc_aval,
  display_vars = exprs(
    USUBJID, NFRLT, AVISIT, ATPT, ATPTREF, AVAL, AVALCAT1
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Create DTYPE imputation

adpc_lloq <- adpc_aval %>%
  derive_extreme_records(
    dataset_add = adpc_aval,
    by_vars = exprs(USUBJID, PARAMCD, PARCAT1, AVISITN, AVISIT, ADTM, PCSEQ),
    order = exprs(ADTM, BASETYPE, EVID, ATPTN, PARCAT1),
    mode = "last",
    filter_add = PCSTRESC == "<BLQ" & is.na(AVAL),
    set_values_to = exprs(
      AVAL = ALLOQ * .5,
      DTYPE = "HALFLLOQ"
    )
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  tail(adpc_lloq),
  display_vars = exprs(
    USUBJID, PARAMCD, PARCAT1, AVISITN, DTYPE, PCSTRESC, AVAL,
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Create DTYPE copy records ----

dtype <- adpc_lloq %>%
  filter(NFRLT > 0 & NXRLT == 0 & EVID == 0 & !is.na(AVISIT_next)) %>%
  select(-PCRFTDT, -PCRFTTM) %>%
  # Re-derive variables in for DTYPE copy records
  mutate(
    ATPTREF = AVISIT_next,
    ARRLT = AXRLT,
    NRRLT = NXRLT,
    PCRFTDTM = ADTM_next,
    DOSEA = EXDOSE_next,
    BASETYPE = paste(AVISIT_next, "Baseline"),
    ATPT = "Pre-dose",
    ATPTN = -0.5,
    ABLFL = "Y",
    DTYPE = case_when(
      is.na(DTYPE) ~ "COPY",
      DTYPE == "HALFLLOQ" ~ "COPY/HALFLLOQ"
    )
  ) %>%
  derive_vars_dtm_to_dt(exprs(PCRFTDTM)) %>%
  derive_vars_dtm_to_tm(exprs(PCRFTDTM))

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  dtype,
  display_vars = exprs(
    USUBJID, DTYPE, ATPT, NFRLT, NRRLT, AFRLT, ARRLT, BASETYPE
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Combine original records and DTYPE copy records ----

adpc_dtype <- bind_rows(adpc_lloq, dtype) %>%
  arrange(STUDYID, USUBJID, BASETYPE, ADTM, NFRLT) %>%
  mutate(
    # Derive baseline flag for pre-dose records
    ABLFL = case_when(
      ATPT == "Pre-dose" & !is.na(AVAL) ~ "Y",
      TRUE ~ NA_character_
    ),
    # Derive MRRLT, ANL01FL and ANL02FL
    MRRLT = if_else(ARRLT < 0, 0, ARRLT),
    ANL01FL = "Y",
    ANL02FL = if_else(is.na(DTYPE), "Y", NA_character_)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
adpc_dtype %>%
  dataset_vignette(display_vars = exprs(
    STUDYID, USUBJID, ABLFL, BASETYPE, DTYPE, ADTM, ATPT, NFRLT, NRRLT, ARRLT, MRRLT
  ))

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Derive BASE and Calculate Change from Baseline ----

adpc_base <- adpc_dtype %>%
  # Derive BASE
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD, PARCAT1, BASETYPE),
    source_var = AVAL,
    new_var = BASE,
    filter = ABLFL == "Y"
  )

# Calculate CHG for post-baseline records
# The decision on how to populate pre-baseline and baseline values of CHG is left to producer choice
adpc_chg <- restrict_derivation(
  adpc_base,
  derivation = derive_var_chg,
  filter = AVISITN > 0
)

# ---- Add ASEQ ----

adpc_aseq <- adpc_chg %>%
  # Calculate ASEQ
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(ADTM, BASETYPE, EVID, AVISITN, ATPTN, PARCAT1, DTYPE),
    check_type = "error"
  ) %>%
  # Remove temporary variables
  select(
    -DOMAIN, -PCSEQ, -starts_with("orig"), -starts_with("min"),
    -starts_with("max"), -starts_with("EX"), -ends_with("next"),
    -ends_with("prev"), -DRUG, -EVID, -AXRLT, -NXRLT, -VISITDY
  ) %>%
  # Derive PARAM and PARAMN
  derive_vars_merged(
    dataset_add = select(param_lookup, -PCTESTCD), by_vars = exprs(PARAMCD)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
adpc_aseq %>%
  dataset_vignette(display_vars = exprs(
    USUBJID, BASETYPE, DTYPE, AVISIT, ATPT, AVAL, NFRLT, NRRLT, AFRLT, ARRLT, BASE, CHG
  ))

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# Derive additional baselines from VS
adpc_baselines <- adpc_aseq %>%
  derive_vars_merged(
    dataset_add = vs,
    filter_add = VSTESTCD == "HEIGHT",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(HTBL = VSSTRESN, HTBLU = VSSTRESU)
  ) %>%
  derive_vars_merged(
    dataset_add = vs,
    filter_add = VSTESTCD == "WEIGHT" & VSBLFL == "Y",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(WTBL = VSSTRESN, WTBLU = VSSTRESU)
  ) %>%
  mutate(
    BMIBL = compute_bmi(height = HTBL, weight = WTBL),
    BMIBLU = "kg/m^2"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
adpc_baselines %>%
  dataset_vignette(display_vars = exprs(
    USUBJID, HTBL, HTBLU, WTBL, WTBLU, BMIBL, BMIBLU, BASETYPE, ATPT, AVAL
  ))

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# Add all ADSL variables
adpc <- adpc_baselines %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  )

## ----eval=TRUE, echo=TRUE, message=FALSE--------------------------------------
# ---- Find first dose per treatment per subject ----
# ---- Join with ADPC data and keep only subjects with dosing ----

adppk_first_dose <- pc_dates %>%
  derive_vars_merged(
    dataset_add = ex_exp,
    filter_add = (!is.na(ADTM)),
    new_vars = exprs(FANLDTM = ADTM, EXDOSE_first = EXDOSE),
    order = exprs(ADTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID, DRUG)
  ) %>%
  filter(!is.na(FANLDTM)) %>%
  # Derive AVISIT based on nominal relative time
  # Derive AVISITN to nominal time in whole days using integer division
  # Define AVISIT based on nominal day
  mutate(
    AVISITN = NFRLT %/% 24 + 1,
    AVISIT = paste("Day", AVISITN),
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adppk_first_dose,
  display_vars = exprs(
    USUBJID, FANLDTM, AVISIT, ADTM, PCTPT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Find previous dose  ----

adppk_prev <- adppk_first_dose %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(ADTM),
    new_vars = exprs(
      ADTM_prev = ADTM, EXDOSE_prev = EXDOSE, AVISIT_prev = AVISIT,
      AENDTM_prev = AENDTM
    ),
    join_vars = exprs(ADTM),
    join_type = "all",
    filter_add = NULL,
    filter_join = ADTM > ADTM.join,
    mode = "last",
    check_type = "none"
  )

# ---- Find previous nominal dose ----

adppk_nom_prev <- adppk_prev %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(NFRLT),
    new_vars = exprs(NFRLT_prev = NFRLT),
    join_vars = exprs(NFRLT),
    join_type = "all",
    filter_add = NULL,
    filter_join = NFRLT > NFRLT.join,
    mode = "last",
    check_type = "none"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adppk_nom_prev,
  display_vars = exprs(
    USUBJID, VISIT, ADTM, VISIT, PCTPT, ADTM_prev, NFRLT_prev
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Combine ADPPK and EX data ----
# Derive Relative Time Variables

adppk_aprlt <- bind_rows(adppk_nom_prev, ex_exp) %>%
  group_by(USUBJID, DRUG) %>%
  mutate(
    FANLDTM = min(FANLDTM, na.rm = TRUE),
    min_NFRLT = min(NFRLT, na.rm = TRUE),
    maxdate = max(ADT[EVID == 0], na.rm = TRUE), .after = USUBJID
  ) %>%
  arrange(USUBJID, ADTM) %>%
  ungroup() %>%
  filter(ADT <= maxdate) %>%
  # Derive Actual Relative Time from First Dose (AFRLT)
  derive_vars_duration(
    new_var = AFRLT,
    start_date = FANLDTM,
    end_date = ADTM,
    out_unit = "hours",
    floor_in = FALSE,
    add_one = FALSE
  ) %>%
  # Derive Actual Relative Time from Reference Dose (APRLT)
  derive_vars_duration(
    new_var = APRLT,
    start_date = ADTM_prev,
    end_date = ADTM,
    out_unit = "hours",
    floor_in = FALSE,
    add_one = FALSE
  ) %>%
  # Derive APRLT
  mutate(
    APRLT = case_when(
      EVID == 1 ~ 0,
      is.na(APRLT) ~ AFRLT,
      TRUE ~ APRLT
    ),
    NPRLT = case_when(
      EVID == 1 ~ 0,
      is.na(NFRLT_prev) ~ NFRLT - min_NFRLT,
      TRUE ~ NFRLT - NFRLT_prev
    )
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adppk_aprlt,
  display_vars = exprs(
    USUBJID, EVID, NFRLT, AFRLT, APRLT, NPRLT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Derive Analysis Variables ----
# Derive actual dose DOSEA and planned dose DOSEP,
# Derive AVAL and DV

adppk_aval <- adppk_aprlt %>%
  mutate(
    # Derive Actual Dose
    DOSEA = case_when(
      EVID == 1 ~ EXDOSE,
      is.na(EXDOSE_prev) ~ EXDOSE_first,
      TRUE ~ EXDOSE_prev
    ),
    # Derive Planned Dose
    DOSEP = case_when(
      TRT01P == "Xanomeline High Dose" ~ 81,
      TRT01P == "Xanomeline Low Dose" ~ 54,
      TRT01P == "Placebo" ~ 0
    ),
    # Derive PARAMCD
    PARAMCD = case_when(
      EVID == 1 ~ "DOSE",
      TRUE ~ PCTESTCD
    ),
    ALLOQ = PCLLOQ,
    # Derive CMT
    CMT = case_when(
      EVID == 1 ~ 1,
      PCSPEC == "PLASMA" ~ 2,
      TRUE ~ 3
    ),
    # Derive BLQFL/BLQFN
    BLQFL = case_when(
      PCSTRESC == "<BLQ" ~ "Y",
      TRUE ~ "N"
    ),
    BLQFN = case_when(
      PCSTRESC == "<BLQ" ~ 1,
      TRUE ~ 0
    ),
    AMT = case_when(
      EVID == 1 ~ EXDOSE,
      TRUE ~ NA_real_
    ),
    # Derive DV and AVAL
    DV = PCSTRESN,
    AVAL = DV,
    DVL = case_when(
      DV != 0 ~ log(DV),
      TRUE ~ NA_real_
    ),
    # Derive MDV
    MDV = case_when(
      EVID == 1 ~ 1,
      is.na(DV) ~ 1,
      TRUE ~ 0
    ),
    AVALU = case_when(
      EVID == 1 ~ NA_character_,
      TRUE ~ PCSTRESU
    ),
    UDTC = format_ISO8601(ADTM),
    II = if_else(EVID == 1, 1, 0),
    SS = if_else(EVID == 1, 1, 0)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adppk_aval,
  display_vars = exprs(
    USUBJID, EVID, DOSEA, AMT, NFRLT, AFRLT, CMT, DV, MDV, BLQFN
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Add ASEQ ----

adppk_aseq <- adppk_aval %>%
  # Calculate ASEQ
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(AFRLT, EVID, CMT),
    check_type = "error"
  ) %>%
  # Derive PARAM and PARAMN
  derive_vars_merged(dataset_add = select(param_lookup, -PCTESTCD), by_vars = exprs(PARAMCD)) %>%
  mutate(
    PROJID = DRUG,
    PROJIDN = 1
  ) %>%
  # Remove temporary variables
  select(
    -DOMAIN, -starts_with("min"), -starts_with("max"), -starts_with("EX"),
    -starts_with("PC"), -ends_with("first"), -ends_with("prev"),
    -ends_with("DTM"), -ends_with("DT"), -ends_with("TM"), -starts_with("VISIT"),
    -starts_with("AVISIT"), -ends_with("TMF"), -starts_with("TRT"),
    -starts_with("ATPT"), -DRUG
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adppk_aseq,
  display_vars = exprs(
    USUBJID, EVID, DOSEA, AMT, NFRLT, AFRLT, CMT, DV, MDV, BLQFN, ASEQ
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
#---- Derive Covariates ----
# Include numeric values for STUDYIDN, USUBJIDN, SEXN, RACEN etc.

covar <- adsl %>%
  derive_vars_merged(
    dataset_add = country_code_lookup,
    new_vars = exprs(COUNTRYN = country_number, COUNTRYL = country_name),
    by_vars = exprs(COUNTRY = country_code),
  ) %>%
  mutate(
    STUDYIDN = as.numeric(word(USUBJID, 1, sep = fixed("-"))),
    SITEIDN = as.numeric(word(USUBJID, 2, sep = fixed("-"))),
    USUBJIDN = as.numeric(word(USUBJID, 3, sep = fixed("-"))),
    SUBJIDN = as.numeric(SUBJID),
    SEXN = case_when(
      SEX == "M" ~ 1,
      SEX == "F" ~ 2,
      TRUE ~ 3
    ),
    RACEN = case_when(
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
      RACE == "ASIAN" ~ 2,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 3,
      RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 4,
      RACE == "WHITE" ~ 5,
      TRUE ~ 6
    ),
    ETHNICN = case_when(
      ETHNIC == "HISPANIC OR LATINO" ~ 1,
      ETHNIC == "NOT HISPANIC OR LATINO" ~ 2,
      TRUE ~ 3
    ),
    ARMN = case_when(
      ARM == "Placebo" ~ 0,
      ARM == "Xanomeline Low Dose" ~ 1,
      ARM == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    ),
    ACTARMN = case_when(
      ACTARM == "Placebo" ~ 0,
      ACTARM == "Xanomeline Low Dose" ~ 1,
      ACTARM == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    ),
    COHORT = ARMN,
    COHORTC = ARM,
    ROUTE = unique(ex$EXROUTE),
    ROUTEN = case_when(
      ROUTE == "TRANSDERMAL" ~ 3,
      TRUE ~ NA_real_
    ),
    FORM = unique(ex$EXDOSFRM),
    FORMN = case_when(
      FORM == "PATCH" ~ 3,
      TRUE ~ 4
    )
  ) %>%
  select(
    STUDYID, STUDYIDN, SITEID, SITEIDN, USUBJID, USUBJIDN,
    SUBJID, SUBJIDN, AGE, SEX, SEXN, COHORT, COHORTC, ROUTE, ROUTEN,
    RACE, RACEN, ETHNIC, ETHNICN, FORM, FORMN, COUNTRY, COUNTRYN, COUNTRYL
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  covar,
  display_vars = exprs(
    STUDYIDN, USUBJIDN, SITEIDN, COUNTRY, COUNTRYN, AGE, SEXN, RACEN, COHORT, ROUTEN
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
#---- Derive additional baselines from VS and LB ----

labsbl <- lb %>%
  filter(LBBLFL == "Y" & LBTESTCD %in% c("CREAT", "ALT", "AST", "BILI")) %>%
  mutate(LBTESTCDB = paste0(LBTESTCD, "BL")) %>%
  select(STUDYID, USUBJID, LBTESTCDB, LBSTRESN)

covar_vslb <- covar %>%
  derive_vars_merged(
    dataset_add = vs,
    filter_add = VSTESTCD == "HEIGHT",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(HTBL = VSSTRESN)
  ) %>%
  derive_vars_merged(
    dataset_add = vs,
    filter_add = VSTESTCD == "WEIGHT" & VSBLFL == "Y",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(WTBL = VSSTRESN)
  ) %>%
  derive_vars_transposed(
    dataset_merge = labsbl,
    by_vars = exprs(STUDYID, USUBJID),
    key_var = LBTESTCDB,
    value_var = LBSTRESN
  ) %>%
  mutate(
    BMIBL = compute_bmi(height = HTBL, weight = WTBL),
    BSABL = compute_bsa(
      height = HTBL,
      weight = HTBL,
      method = "Mosteller"
    ),
    # Derive CRCLBL and EGFRBL using new function
    CRCLBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE, weight = WTBL, sex = SEX,
      method = "CRCL"
    ),
    EGFRBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE, weight = WTBL, sex = SEX,
      method = "CKD-EPI"
    )
  ) %>%
  rename(TBILBL = BILIBL)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  covar_vslb,
  display_vars = exprs(
    USUBJIDN, AGE, SEXN, HTBL, WTBL, CREATBL, ALTBL, ASTBL
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# Combine covariates with APPPK data

adppk <- adppk_aseq %>%
  derive_vars_merged(
    dataset_add = covar_vslb,
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  arrange(STUDYIDN, USUBJIDN, AFRLT, EVID) %>%
  mutate(RECSEQ = row_number())

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adppk,
  display_vars = exprs(
    USUBJIDN, AGE, SEXN, CREATBL, EVID, AMT, DV, MDV, RECSEQ
  )
)

