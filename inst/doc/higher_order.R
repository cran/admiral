## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(admiral)
library(pharmaversesdtm)
library(dplyr, warn.conflicts = FALSE)

data("admiral_adsl")
data("ae")
data("vs")
adsl <- admiral_adsl
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

## ----echo=FALSE---------------------------------------------------------------
adsl <- filter(adsl, USUBJID %in% c("01-701-1111", "01-705-1393"))
ae <- filter(ae, USUBJID %in% c("01-701-1111", "01-705-1393"))
vs <- filter(vs, USUBJID %in% c("01-701-1015"))

## -----------------------------------------------------------------------------
adae <- ae %>%
  left_join(adsl, by = c("STUDYID", "USUBJID")) %>%
  derive_vars_dt(
    new_vars_prefix = "AST",
    dtc = AESTDTC,
    highest_imputation = "M"
  ) %>%
  mutate(TRTEMFL = if_else(ASTDT >= TRTSDT, "Y", NA_character_))

## -----------------------------------------------------------------------------
vs_without <- vs %>%
  derive_var_extreme_flag(
    by_vars = exprs(USUBJID, VSTESTCD),
    order = exprs(VSORRES, VSSEQ),
    new_var = AHIFL,
    mode = "last"
  ) %>%
  derive_var_extreme_flag(
    by_vars = exprs(USUBJID, VSTESTCD),
    order = exprs(VSORRES, VSSEQ),
    new_var = ALOFL,
    mode = "first"
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
vs_without %>%
  arrange(USUBJID, VSTESTCD, VSDY, VSSEQ) %>%
  dataset_vignette(
    display_vars = exprs(USUBJID, VSTESTCD, VSORRES, ALOFL, AHIFL),
    filter = VSTESTCD %in% c("TEMP", "WEIGHT")
  )

## -----------------------------------------------------------------------------
vs_with <- vs %>%
  call_derivation(
    derivation = derive_var_extreme_flag,
    variable_params = list(
      params(new_var = AHIFL, mode = "last"),
      params(new_var = ALOFL, mode = "first")
    ),
    by_vars = exprs(USUBJID, VSTESTCD),
    order = exprs(VSORRES, VSSEQ)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
vs_with %>%
  arrange(USUBJID, VSTESTCD, VSDY, VSSEQ) %>%
  dataset_vignette(
    display_vars = exprs(USUBJID, VSTESTCD, VSORRES, ALOFL, AHIFL),
    filter = VSTESTCD %in% c("TEMP", "WEIGHT")
  )

## -----------------------------------------------------------------------------
adaette <- call_derivation(
  derivation = derive_param_tte,
  variable_params = list(
    params(
      event_conditions = list(ae_event),
      set_values_to = exprs(PARAMCD = "TTAE")
    ),
    params(
      event_conditions = list(ae_ser_event),
      set_values_to = exprs(PARAMCD = "TTSERAE")
    ),
    params(
      event_conditions = list(ae_sev_event),
      set_values_to = exprs(PARAMCD = "TTSEVAE")
    ),
    params(
      event_conditions = list(ae_wd_event),
      set_values_to = exprs(PARAMCD = "TTWDAE")
    )
  ),
  dataset_adsl = adsl,
  source_datasets = list(adsl = adsl, adae = adae),
  censor_conditions = list(lastalive_censor)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
adaette %>%
  select(USUBJID, PARAMCD, STARTDT, ADT, CNSR, EVNTDESC, SRCDOM, SRCVAR) %>%
  arrange(USUBJID, PARAMCD) %>%
  dataset_vignette(display_vars = exprs(USUBJID, PARAMCD, STARTDT, ADT, CNSR, EVNTDESC, SRCDOM, SRCVAR))

## -----------------------------------------------------------------------------
ae <- ae %>%
  mutate(TEMP_AESEVN = as.integer(factor(AESEV, levels = c("SEVERE", "MODERATE", "MILD")))) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = AHSEVFL,
      by_vars = exprs(USUBJID),
      order = exprs(TEMP_AESEVN, AESTDY, AESEQ),
      mode = "first"
    ),
    filter = AESTDY >= 1
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
ae %>%
  arrange(USUBJID, AESTDY, AESEQ, desc(TEMP_AESEVN)) %>%
  dataset_vignette(
    display_vars = exprs(USUBJID, AEDECOD, AESTDY, AESEQ, AESEV, AHSEVFL)
  )

## -----------------------------------------------------------------------------
ae <- ae %>%
  slice_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = AHSEV2FL,
      by_vars = exprs(USUBJID)
    ),
    derivation_slice(
      filter = AESTDY >= 1,
      args = params(order = exprs(TEMP_AESEVN, AESTDY, AESEQ), mode = "first")
    ),
    derivation_slice(
      filter = TRUE,
      args = params(order = exprs(AESTDY, AESEQ), mode = "last")
    )
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
ae %>%
  arrange(USUBJID, AESTDY, AESEQ, desc(TEMP_AESEVN)) %>%
  dataset_vignette(
    display_vars = exprs(USUBJID, AEDECOD, AESTDY, AESEQ, AESEV, AHSEV2FL)
  )

## -----------------------------------------------------------------------------
ae <- ae %>%
  slice_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = AHSEV3FL,
      by_vars = exprs(USUBJID)
    ),
    derivation_slice(
      filter = AESEV == "SEVERE",
      args = params(order = exprs(AESTDY, AESEQ), mode = "first")
    ),
    derivation_slice(
      filter = AESEV == "MODERATE",
      args = params(order = exprs(AESTDY, AESEQ), mode = "first")
    ),
    derivation_slice(
      filter = TRUE,
      args = params(order = exprs(AESTDY, AESEQ), mode = "last")
    )
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
ae %>%
  arrange(USUBJID, AESTDY, AESEQ) %>%
  dataset_vignette(
    display_vars = exprs(USUBJID, AEDECOD, AESTDY, AESEQ, AESEV, AHSEV3FL)
  )

