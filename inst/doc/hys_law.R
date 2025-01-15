## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiral)
library(admiraldev)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(admiral)
library(dplyr, warn.conflicts = FALSE)

## ----eval = TRUE--------------------------------------------------------------
adlb <- admiral::admiral_adlb %>%
  filter(PARAMCD %in% c("AST", "ALT", "BILI") & is.na(DTYPE))

## ----echo = FALSE-------------------------------------------------------------
head(adlb) %>%
  dataset_vignette()

## -----------------------------------------------------------------------------
adlb_annotated <- adlb %>%
  slice_derivation(
    derive_vars_crit_flag,
    args = params(
      values_yn = TRUE
    ),
    derivation_slice(
      filter = PARAMCD %in% c("AST", "ALT"),
      args = params(
        condition = AVAL / ANRHI >= 3,
        description = paste(PARAMCD, ">=3xULN")
      )
    ),
    derivation_slice(
      filter = PARAMCD == "BILI",
      args = params(
        condition = AVAL / ANRHI >= 2,
        description = "BILI >= 2xULN"
      )
    )
  ) %>%
  select(STUDYID, USUBJID, TRT01A, PARAMCD, LBSEQ, ADT, AVISIT, ADY, AVAL, ANRHI, CRIT1, CRIT1FL)

## ----echo = FALSE-------------------------------------------------------------
dataset_vignette(
  adlb_annotated,
  display_vars = exprs(USUBJID, PARAMCD, AVISIT, AVAL, ANRHI, CRIT1, CRIT1FL)
)

## ----warning = FALSE----------------------------------------------------------
altast_records <- adlb_annotated %>%
  filter(PARAMCD %in% c("AST", "ALT"))

bili_records <- adlb_annotated %>%
  filter(PARAMCD %in% c("BILI"))

hylaw_records <- derive_vars_joined(
  dataset = altast_records,
  dataset_add = bili_records,
  by_vars = exprs(STUDYID, USUBJID),
  order = exprs(ADY),
  join_type = "all",
  filter_join = 0 <= ADT.join - ADT & ADT.join - ADT <= 14 & CRIT1FL == "Y" & CRIT1FL.join == "Y",
  new_vars = exprs(BILI_DT = ADT, BILI_CRITFL = CRIT1FL),
  mode = "first"
)

## ----echo = FALSE-------------------------------------------------------------
hylaw_records %>%
  arrange(desc(BILI_CRITFL), desc(CRIT1FL)) %>%
  dataset_vignette(display_vars = exprs(USUBJID, PARAMCD, AVISIT, ADT, CRIT1FL, BILI_DT, BILI_CRITFL))

## -----------------------------------------------------------------------------
hylaw_records_pts_visits <- hylaw_records %>%
  select(STUDYID, USUBJID, TRT01A) %>% # add AVISIT, ADT for by visit
  distinct()

hylaw_records_fls <- hylaw_records %>%
  select(STUDYID, USUBJID, TRT01A, CRIT1FL, BILI_CRITFL) %>% # add AVISIT, ADT for by visit
  distinct()

hylaw_params <- derive_param_exist_flag(
  dataset_ref = hylaw_records_pts_visits,
  dataset_add = hylaw_records_fls,
  condition = CRIT1FL == "Y" & BILI_CRITFL == "Y",
  false_value = "N",
  missing_value = "N",
  by_vars = exprs(STUDYID, USUBJID, TRT01A), # add AVISIT, ADT for by visit
  set_values_to = exprs(
    PARAMCD = "HYSLAW",
    PARAM = "ALT/AST >= 3xULN and BILI >= 2xULN",
    AVAL = yn_to_numeric(AVALC)
  )
)

## ----echo = FALSE-------------------------------------------------------------
hylaw_params %>%
  arrange(desc(AVAL)) %>%
  relocate(AVALC, .before = AVAL) %>%
  dataset_vignette(display_vars = exprs(USUBJID, PARAMCD, PARAM, AVALC, AVAL))

## -----------------------------------------------------------------------------
adlbhy <- adlb_annotated %>%
  bind_rows(hylaw_params)

## ----echo = FALSE-------------------------------------------------------------
dataset_vignette(
  adlbhy %>% relocate(AVALC, .before = AVAL),
  display_vars = exprs(USUBJID, PARAMCD, AVISIT, AVALC, AVAL, CRIT1, CRIT1FL)
)

