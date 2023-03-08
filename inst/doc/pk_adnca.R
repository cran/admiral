## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ----message=FALSE------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(admiral)
library(admiral.test)
library(lubridate)
library(stringr)
library(tibble)

data("admiral_adsl")
data("admiral_ex")
data("admiral_pc")
data("admiral_vs")

adsl <- admiral_adsl
ex <- convert_blanks_to_na(admiral_ex)

# Load PC

pc <- convert_blanks_to_na(admiral_pc)

# Load VS for baseline height and weight

vs <- convert_blanks_to_na(admiral_vs)

# ---- Lookup tables ----
param_lookup <- tibble::tribble(
  ~PCTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "XAN", "XAN", "Pharmacokinetic concentration of Xanomeline", 1,
  "DOSE", "DOSE", "Xanomeline Patch Dose", 2,
)

## ----echo=FALSE---------------------------------------------------------------
ex <- filter(ex, USUBJID %in% c("01-701-1028", "01-701-1033", "01-701-1442", "01-714-1288", "01-718-1101"))
pc <- filter(pc, USUBJID %in% c("01-701-1028", "01-701-1033", "01-701-1442", "01-714-1288", "01-718-1101"))

## ----eval=TRUE----------------------------------------------------------------

adsl_vars <- exprs(TRTSDT, TRTSDTM, TRT01P, TRT01A)

adpc <- pc %>%
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

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adpc,
  display_vars = exprs(
    USUBJID, PCTEST, ADTM, VISIT, PCTPT, NFRLT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------

# ---- Get dosing information ----

ex <- ex %>%
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
    NFRLT = 24 * (VISITDY - 1), .after = USUBJID
  ) %>%
  # Set missing end dates to start date
  mutate(AENDTM = case_when(
    is.na(AENDTM) ~ ASTDTM,
    TRUE ~ AENDTM
  )) %>%
  # Derive dates from date/times
  derive_vars_dtm_to_dt(exprs(ASTDTM)) %>%
  derive_vars_dtm_to_dt(exprs(AENDTM))

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  ex,
  display_vars = exprs(
    USUBJID, EXTRT, EXDOSFRQ, ASTDTM, AENDTM, VISIT, VISITDY, NFRLT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Expand dosing records between start and end dates ----

ex_exp <- ex %>%
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
      VISIT, VISITNUM, VISITDY,
      TRT01A, TRT01P, DOMAIN, EXSEQ, !!!adsl_vars
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

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  ex_exp,
  display_vars = exprs(USUBJID, DRUG, EXDOSFRQ, ASTDTM, AENDTM, AVISIT, NFRLT)
)

## ----eval=TRUE, echo=TRUE, message=FALSE--------------------------------------
# ---- Find first dose per treatment per subject ----
# ---- Join with ADPC data and keep only subjects with dosing ----

adpc <- adpc %>%
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
    AVISIT = paste("Day", AVISITN),
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adpc,
  display_vars = exprs(USUBJID, FANLDTM, AVISIT, ADTM, PCTPT)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Find previous dose  ----
# Use derive_vars_joined() for consistency with other variables
# This is equivalent to derive_vars_last_dose() in this case

adpc <- adpc %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(ADTM),
    new_vars = exprs(
      ADTM_prev = ADTM, EXDOSE_prev = EXDOSE, AVISIT_prev = AVISIT,
      AENDTM_prev = AENDTM
    ),
    join_vars = exprs(ADTM),
    filter_add = NULL,
    filter_join = ADTM > ADTM.join,
    mode = "last",
    check_type = "none"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adpc,
  display_vars = exprs(
    USUBJID,
    VISIT, ADTM, VISIT, PCTPT, ADTM_prev, EXDOSE_prev, AVISIT_prev
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Find next dose  ----

adpc <- adpc %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(ADTM),
    new_vars = exprs(
      ADTM_next = ADTM, EXDOSE_next = EXDOSE, AVISIT_next = AVISIT,
      AENDTM_next = AENDTM
    ),
    join_vars = exprs(ADTM),
    filter_add = NULL,
    filter_join = ADTM <= ADTM.join,
    mode = "first",
    check_type = "none"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adpc,
  display_vars = exprs(
    USUBJID,
    VISIT, ADTM, VISIT, PCTPT, ADTM_next, EXDOSE_next, AVISIT_next
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Find previous nominal time ----

adpc <- adpc %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(NFRLT),
    new_vars = exprs(NFRLT_prev = NFRLT),
    join_vars = exprs(NFRLT),
    filter_add = NULL,
    filter_join = NFRLT > NFRLT.join,
    mode = "last",
    check_type = "none"
  )

# ---- Find next nominal time ----

adpc <- adpc %>%
  derive_vars_joined(
    dataset_add = ex_exp,
    by_vars = exprs(USUBJID),
    order = exprs(NFRLT),
    new_vars = exprs(NFRLT_next = NFRLT),
    join_vars = exprs(NFRLT),
    filter_add = NULL,
    filter_join = NFRLT <= NFRLT.join,
    mode = "first",
    check_type = "none"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adpc,
  display_vars = exprs(
    USUBJID, NFRLT, PCTPT, NFRLT_prev, NFRLT_next
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------

# ---- Combine ADPC and EX data ----
# Derive Relative Time Variables


adpc <- bind_rows(adpc, ex_exp) %>%
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

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adpc,
  display_vars = exprs(USUBJID, FANLDTM, AVISIT, PCTPT, AFRLT, ARRLT, AXRLT)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------

adpc <- adpc %>%
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

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adpc,
  display_vars = exprs(USUBJID, AVISIT, PCTPT, NFRLT, NRRLT, NXRLT)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Derive Analysis Variables ----
# Derive ATPTN, ATPT, ATPTREF, ABLFL and BASETYPE
# Derive planned dose DOSEP, actual dose DOSEA and units
# Derive PARAMCD and relative time units
# Derive AVAL, AVALU and AVALCAT1

adpc <- adpc %>%
  mutate(
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
    # Derive baseline flag for pre-dose records
    ABLFL = case_when(
      ATPT == "Pre-dose" ~ "Y",
      TRUE ~ NA_character_
    ),
    # Derive BASETYPE
    BASETYPE = paste(ATPTREF, "Baseline"),

    # Derive Actual Dose
    DOSEA = case_when(
      EVID == 1 ~ EXDOSE,
      is.na(EXDOSE_prev) ~ EXDOSE_next,
      TRUE ~ EXDOSE_next
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
      PCSTRESC == "<BLQ" & NFRLT == 0 ~ 0,
      PCSTRESC == "<BLQ" & NFRLT > 0 ~ 0.5 * ALLOQ,
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

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adpc,
  display_vars = exprs(USUBJID, NFRLT, AVISIT, ATPT, ABLFL, ATPTREF, AVAL, AVALCAT1)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Create DTYPE copy records ----

dtype <- adpc %>%
  filter(NFRLT > 0 & NXRLT == 0 & EVID == 0 & !is.na(AVISIT_next)) %>%
  select(-PCRFTDT, -PCRFTTM) %>%
  # Re-derive variables in for DTYPE copy records
  mutate(
    ABLFL = NA_character_,
    ATPTREF = AVISIT_next,
    ARRLT = AXRLT,
    NRRLT = NXRLT,
    PCRFTDTM = ADTM_next,
    DOSEA = EXDOSE_next,
    BASETYPE = paste(AVISIT_next, "Baseline"),
    ATPT = "Pre-dose",
    ATPTN = NFRLT,
    ABLFL = "Y",
    DTYPE = "COPY"
  ) %>%
  derive_vars_dtm_to_dt(exprs(PCRFTDTM)) %>%
  derive_vars_dtm_to_tm(exprs(PCRFTDTM))

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  dtype,
  display_vars = exprs(USUBJID, DTYPE, ATPT, NFRLT, NRRLT, AFRLT, ARRLT, BASETYPE)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# ---- Combine original records and DTYPE copy records ----

adpc <- bind_rows(adpc, dtype) %>%
  arrange(STUDYID, USUBJID, BASETYPE, ADTM, NFRLT) %>%
  mutate(
    # Derive MRRLT, ANL01FL and ANL02FL
    MRRLT = if_else(ARRLT < 0, 0, ARRLT),
    ANL01FL = "Y",
    ANL02FL = if_else(is.na(DTYPE), "Y", NA_character_),
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
adpc %>%
  dataset_vignette(display_vars = exprs(STUDYID, USUBJID, BASETYPE, ADTM, ATPT, NFRLT, NRRLT, ARRLT, MRRLT))

## ----eval=TRUE, echo=TRUE-----------------------------------------------------

# ---- Derive BASE and Calculate Change from Baseline ----

adpc <- adpc %>%
  # Derive BASE
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD, BASETYPE),
    source_var = AVAL,
    new_var = BASE,
    filter = ABLFL == "Y"
  )

adpc <- derive_var_chg(adpc)

# ---- Add ASEQ ----

adpc <- adpc %>%
  # Calculate ASEQ
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(ADTM, BASETYPE, EVID, AVISITN, ATPTN, DTYPE),
    check_type = "error"
  ) %>%
  # Remove temporary variables
  select(
    -DOMAIN, -PCSEQ, -starts_with("orig"), -starts_with("min"),
    -starts_with("max"), -starts_with("EX"), -ends_with("next"),
    -ends_with("prev"), -DRUG, -EVID, -AXRLT, -NXRLT, -VISITDY
  ) %>%
  # Derive PARAM and PARAMN
  derive_vars_merged(dataset_add = select(param_lookup, -PCTESTCD), by_vars = exprs(PARAMCD))

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
adpc %>%
  dataset_vignette(display_vars = exprs(
    USUBJID, BASETYPE, DTYPE, AVISIT, ATPT, AVAL, NFRLT, NRRLT, AFRLT, ARRLT, BASE, CHG
  ))

## ----eval=TRUE, echo=TRUE-----------------------------------------------------

# Derive additional baselines from VS
adpc <- adpc %>%
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

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
adpc %>%
  dataset_vignette(display_vars = exprs(
    USUBJID, HTBL, HTBLU, WTBL, WTBLU, BMIBL, BMIBLU, BASETYPE, ATPT, AVAL
  ))

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
# Add all ADSL variables
adpc <- adpc %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  )

