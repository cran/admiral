## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(admiraldev)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(admiral)
library(admiral.test)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(tibble)

data("admiral_dm")
data("admiral_ds")
data("admiral_ex")
data("admiral_ae")
dm <- convert_blanks_to_na(admiral_dm)
ds <- convert_blanks_to_na(admiral_ds)
ex <- convert_blanks_to_na(admiral_ex)
ae <- convert_blanks_to_na(admiral_ae)

## ----echo=FALSE---------------------------------------------------------------
# Filter test patients and make more realistic and interesting for the examples
dm <- filter(dm, USUBJID %in% c("01-701-1111", "01-701-1047", "01-701-1057"))
ds <- filter(ds, USUBJID %in% c("01-701-1111", "01-701-1047", "01-701-1057")) %>%
  mutate(DSSTDTC = case_when(
    USUBJID == "01-701-1111" & DSDECOD == "RANDOMIZED" ~ "2012-08-01",
    TRUE ~ DSSTDTC
  ))
ex <- filter(ex, USUBJID %in% c("01-701-1111", "01-701-1047", "01-701-1057"))
ae <- filter(ae, USUBJID %in% c("01-701-1111", "01-701-1047")) %>%
  mutate(AESTDTC = case_when(
    USUBJID == "01-701-1111" & AESTDY == "-61" ~ "2012-09-14",
    TRUE ~ AESTDTC
  )) %>%
  mutate(AESTDY = case_when(
    USUBJID == "01-701-1111" & AESTDY == "-61" ~ 8,
    TRUE ~ AESTDY
  ))

## ----eval=TRUE----------------------------------------------------------------
# Use DM domain as basis to build ADSL
adsl_01 <- dm %>%
  select(-DOMAIN)

# Convert disposition character date to numeric date and
# join as randomization date to ADSL
adsl_02 <- adsl_01 %>%
  derive_vars_merged(
    dataset_add = ds,
    filter_add = DSDECOD == "RANDOMIZED",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(RANDDT = convert_dtc_to_dt(DSSTDTC))
  )

## ----eval=TRUE----------------------------------------------------------------
# Convert exposure start date to numeric date without imputation,
# determine first exposure datetime and add to ADSL
adsl_03 <- adsl_02 %>%
  derive_vars_merged(
    dataset_add = ex,
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(TRTSDT),
    new_vars = exprs(TRTSDT = convert_dtc_to_dt(EXSTDTC)),
    order = exprs(TRTSDT, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )

## ----eval=TRUE----------------------------------------------------------------
# Add safety population flag to ADSL
adsl_04 <- adsl_03 %>%
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))),
    false_value = "N",
    missing_value = "N"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  dataset = adsl_04,
  display_vars = exprs(USUBJID, RANDDT, TRTSDT, SAFFL)
)

## ----eval=TRUE----------------------------------------------------------------
# Convert disposition character date to numeric date without imputation
ds_ext <- derive_vars_dt(
  dataset = ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

# Join randomization date to ADSL only for safety population patients
adsl_05 <- adsl_04 %>%
  derive_vars_joined(
    dataset_add = ds_ext,
    filter_add = DSDECOD == "RANDOMIZED",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(RAND30DT = DSSTDT),
    filter_join = DSSTDT >= TRTSDT - 30
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  dataset = adsl_05,
  display_vars = exprs(USUBJID, RANDDT, TRTSDT, RAND30DT)
)

## ----eval=TRUE----------------------------------------------------------------
# Create a unique datacut day for each patient
datacut <- tribble(
  ~USUBJID,      ~DCUTDY, ~DCUTFL,
  "01-701-1047",      25, "Y",
  "01-701-1111",       5, "Y"
)

# Join datacut flag to AE only for events up to and including this date
ae_01 <- ae %>%
  derive_vars_joined(
    dataset_add = datacut,
    by_vars = exprs(USUBJID),
    new_vars = exprs(DCUTFL),
    join_vars = exprs(DCUTDY),
    filter_join = AESTDY <= DCUTDY
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
ae_01 %>%
  select(USUBJID, AEDECOD, AESTDY, DCUTFL) %>%
  arrange(USUBJID, AESTDY) %>%
  dataset_vignette(display_vars = exprs(USUBJID, AEDECOD, AESTDY, DCUTFL))

## ----eval=TRUE----------------------------------------------------------------
# Derive nadir severity (AENADSEV)
# Use a numeric version of severity for sorting with severe=1, moderate=2, mild=3
ae_02 <- ae_01 %>%
  derive_vars_joined(
    dataset_add = ae_01,
    filter_add = AESTDY > 0,
    by_vars = exprs(USUBJID),
    order = exprs(as.integer(factor(AESEV, levels = c("SEVERE", "MODERATE", "MILD")))),
    new_vars = exprs(AENADSEV = AESEV),
    join_vars = exprs(AESTDY),
    filter_join = AESTDY.join < AESTDY,
    mode = "first",
    check_type = "none"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
ae_02 %>%
  select(USUBJID, AEDECOD, AESTDY, AESEV, AENADSEV) %>%
  arrange(USUBJID, AESTDY) %>%
  dataset_vignette(display_vars = exprs(USUBJID, AEDECOD, AESTDY, AESEV, AENADSEV))

## ----eval=TRUE----------------------------------------------------------------
# Highest severity flag (AEHSEVFL)
ae_03 <- ae_02 %>%
  derive_var_extreme_flag(
    new_var = AEHSEVFL,
    by_vars = exprs(USUBJID),
    order = exprs(
      as.integer(factor(AESEV, levels = c("SEVERE", "MODERATE", "MILD"))),
      AESTDY, AESEQ
    ),
    mode = "first"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
ae_03 %>%
  select(USUBJID, AESTDY, AESEQ, AESEV, AEHSEVFL) %>%
  arrange(USUBJID, AESTDY, AESEQ) %>%
  dataset_vignette(display_vars = exprs(USUBJID, AESTDY, AESEQ, AESEV, AEHSEVFL))

