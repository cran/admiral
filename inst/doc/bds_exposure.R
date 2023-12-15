## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ----message=FALSE------------------------------------------------------------
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(tibble)

data("admiral_adsl")
data("ex")

adsl <- admiral_adsl
ex <- convert_blanks_to_na(ex)

## ----echo=FALSE---------------------------------------------------------------
ex <- filter(ex, USUBJID %in% c("01-701-1015", "01-701-1023", "01-703-1086", "01-703-1096", "01-707-1037", "01-716-1024"))

## ----eval=TRUE----------------------------------------------------------------
adsl_vars <- exprs(TRTSDT, TRTSDTM, TRTEDT, TRTEDTM)

adex <- derive_vars_merged(
  ex,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = exprs(STUDYID, USUBJID)
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adex,
  display_vars = exprs(
    USUBJID, EXTRT, EXDOSE, EXDOSFRQ,
    VISIT, EXSTDTC, EXENDTC,
    TRTSDTM, TRTEDTM
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- adex %>%
  mutate(
    EXADJ = case_when(
      USUBJID == "01-701-1028" & VISIT %in% c("WEEK 2") ~ "ADVERSE EVENT",
      USUBJID == "01-701-1148" & VISIT %in% c("WEEK 2", "WEEK 24") ~ "MEDICATION ERROR",
      TRUE ~ NA_character_
    ),
    EXDOSE = case_when(
      USUBJID == "01-701-1028" & VISIT %in% c("WEEK 2") ~ 0,
      USUBJID == "01-701-1148" & VISIT %in% c("WEEK 2", "WEEK 24") ~ 0,
      TRUE ~ EXDOSE
    )
  ) %>%
  mutate(EXPLDOS = if_else(EXTRT == "PLACEBO", 0, 54))

distinct(adex, EXTRT, EXPLDOS)
count(adex, EXADJ)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- derive_vars_dt(adex, new_vars_prefix = "AST", dtc = EXSTDTC)
adex <- derive_vars_dt(adex, new_vars_prefix = "AEN", dtc = EXENDTC)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adex,
  display_vars = exprs(USUBJID, VISIT, EXSTDTC, EXENDTC, ASTDT, AENDT)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- derive_vars_dtm(
  adex,
  dtc = EXSTDTC,
  highest_imputation = "M",
  new_vars_prefix = "AST"
)

adex <- derive_vars_dtm(
  adex,
  dtc = EXENDTC,
  highest_imputation = "M",
  date_imputation = "last",
  new_vars_prefix = "AEN"
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adex,
  display_vars = exprs(USUBJID, VISIT, EXSTDTC, EXENDTC, ASTDTM, AENDTM)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <-
  derive_vars_dy(adex,
    reference_date = TRTSDT,
    source_vars = exprs(ASTDT, AENDT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adex,
  display_vars = exprs(
    USUBJID,
    VISIT, ASTDT, ASTDY, AENDT, AENDY, TRTSDT
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- adex %>%
  derive_vars_duration(
    new_var = EXDURD,
    start_date = ASTDT,
    end_date = AENDT
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adex,
  display_vars = exprs(
    USUBJID,
    VISIT, ASTDT, ASTDY, AENDT, AENDY, EXDURD
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- adex %>%
  derive_vars_duration(
    new_var = EXDURDY,
    out_unit = "years",
    start_date = ASTDT,
    end_date = AENDT
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adex,
  display_vars = exprs(
    USUBJID,
    VISIT, ASTDT, AENDT, EXDURD, EXDURDY
  )
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- adex %>%
  mutate(
    DOSEO = EXDOSE * EXDURD,
    PDOSEO = EXPLDOS * EXDURD
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adex,
  display_vars = exprs(USUBJID, EXDOSE, EXPLDOS, EXDURD, DOSEO, PDOSEO)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
single_dose <- adex %>%
  filter(USUBJID == "01-701-1015" & EXSTDY == 1) %>%
  create_single_dose_dataset(keep_source_vars = exprs(USUBJID, EXDOSE, EXPLDOS, EXDOSFRQ, ASTDT, AENDT))

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  single_dose,
  display_vars = exprs(USUBJID, EXDOSE, EXPLDOS, EXDOSFRQ, ASTDT, AENDT)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex_durd <- adex %>%
  mutate(
    PARAMCD = "DURD",
    AVAL = EXDURD
  )

adex_dose <- adex %>%
  mutate(
    PARAMCD = "DOSE",
    AVAL = DOSEO
  )

adex_pldos <- adex %>%
  mutate(
    PARAMCD = "PLDOSE",
    AVAL = PDOSEO
  )

adex_adj <- adex %>%
  mutate(
    PARAMCD = "ADJ",
    AVALC = if_else(!is.na(EXADJ), "Y", NA_character_)
  )

adex_adjae <- adex %>%
  mutate(
    PARAMCD = "ADJAE",
    AVALC = if_else(EXADJ == "ADVERSE EVENT", "Y", NA_character_)
  )

adex <- bind_rows(
  adex_durd,
  adex_dose,
  adex_pldos,
  adex_adj,
  adex_adjae
) %>%
  mutate(PARCAT1 = "INDIVIDUAL")

count(adex, PARAMCD)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
adex %>%
  arrange(USUBJID, VISIT, desc(PARAMCD), EXSTDTC, EXENDTC) %>%
  dataset_vignette(display_vars = exprs(USUBJID, VISIT, ASTDT, AENDT, PARAMCD, AVAL, AVALC))

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- derive_param_exposure(
  adex,
  dataset_add = adex,
  by_vars = exprs(STUDYID, USUBJID, !!!adsl_vars),
  input_code = "DOSE",
  analysis_var = AVAL,
  set_values_to = exprs(PARAMCD = "TDOSE", PARCAT1 = "OVERALL"),
  summary_fun = function(x) sum(x, na.rm = TRUE)
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
adex %>%
  arrange(USUBJID, PARAMCD, PARCAT1, VISIT, EXSTDTC, EXENDTC) %>%
  dataset_vignette(display_vars = exprs(
    USUBJID, VISIT,
    PARCAT1, PARAMCD, AVAL, ASTDT, AENDT
  ))

## ----eval=TRUE, echo=FALSE----------------------------------------------------
adex <- filter(adex, PARAMCD != "TDOSE")

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- adex %>%
  call_derivation(
    derivation = derive_param_exposure,
    variable_params = list(
      params(
        set_values_to = exprs(PARAMCD = "TDOSE", PARCAT1 = "OVERALL"),
        input_code = "DOSE",
        analysis_var = AVAL,
        summary_fun = function(x) sum(x, na.rm = TRUE)
      ),
      params(
        set_values_to = exprs(PARAMCD = "TPDOSE", PARCAT1 = "OVERALL"),
        input_code = "PLDOSE",
        analysis_var = AVAL,
        summary_fun = function(x) sum(x, na.rm = TRUE)
      ),
      params(
        set_values_to = exprs(PARAMCD = "TDURD", PARCAT1 = "OVERALL"),
        input_code = "DURD",
        analysis_var = AVAL,
        summary_fun = function(x) sum(x, na.rm = TRUE)
      ),
      params(
        set_values_to = exprs(PARAMCD = "TADJ", PARCAT1 = "OVERALL"),
        input_code = "ADJ",
        analysis_var = AVALC,
        summary_fun = function(x) if_else(sum(!is.na(x)) > 0, "Y", NA_character_)
      ),
      params(
        set_values_to = exprs(PARAMCD = "TADJAE", PARCAT1 = "OVERALL"),
        input_code = "ADJAE",
        analysis_var = AVALC,
        summary_fun = function(x) if_else(sum(!is.na(x)) > 0, "Y", NA_character_)
      )
    ),
    dataset_add = adex,
    by_vars = exprs(STUDYID, USUBJID, !!!adsl_vars)
  )

count(adex, PARAMCD, PARCAT1)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
adex %>%
  arrange(USUBJID, PARAMCD, PARCAT1, VISIT, EXSTDTC, EXENDTC) %>%
  dataset_vignette(display_vars = exprs(
    USUBJID, VISIT,
    PARCAT1, PARAMCD, AVAL, AVALC, ASTDT, AENDT
  ))

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- adex %>%
  derive_param_doseint(
    by_vars = exprs(STUDYID, USUBJID, !!!adsl_vars),
    set_values_to = exprs(PARAMCD = "TNDOSINT"),
    tadm_code = "TDOSE",
    tpadm_code = "TPDOSE"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adex,
  display_vars = exprs(
    USUBJID, VISIT, EXSTDTC, EXENDTC,
    PARCAT1, PARAMCD, AVAL, ASTDT, AENDT
  )
)

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
param_lookup <- tribble(
  ~PARAMCD, ~PARAM, ~PARAMN,
  "DURD", "Study drug duration during constant dosing interval (days)", 1,
  "DOSE", "Dose administered during constant dosing interval (mg)", 2,
  "PLDOSE", "Planned dose during constant dosing interval (mg)", 3,
  "ADJ", "Dose adjusted during constant dosing interval", 4,
  "ADJAE", "Dose adjusted  due to AE during constant dosing interval", 5,
  "TDURD", "Overall duration (days)", 6,
  "TDOSE", "Total dose administered (mg)", 7,
  "TPDOSE", "Total planned dose (mg)", 9,
  "TADJ", "Dose adjusted during study", 10,
  "TADJAE", "Dose adjusted during study due to AE", 11,
  "TNDOSINT", "Overall dose intensity (%)", 12
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- derive_vars_merged(
  adex,
  dataset_add = param_lookup,
  by_vars = exprs(PARAMCD)
)

count(adex, PARAMCD, PARAM, PARAMN)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- adex %>%
  mutate(
    AVALCAT1 = case_when(
      PARAMCD %in% c("TDURD") & AVAL < 30 ~ "< 30 days",
      PARAMCD %in% c("TDURD") & AVAL >= 30 & AVAL < 90 ~ ">= 30 and < 90 days",
      PARAMCD %in% c("TDURD") & AVAL >= 90 ~ ">=90 days",
      PARAMCD %in% c("TDOSE", "TPDOSE") & AVAL < 1000 ~ "< 1000 mg",
      PARAMCD %in% c("TDOSE", "TPDOSE") & AVAL >= 1000 ~ ">= 1000 mg",
      TRUE ~ NA_character_
    )
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
adex %>%
  arrange(USUBJID, AVALCAT1, PARCAT1, VISIT, EXSTDTC, EXENDTC) %>%
  dataset_vignette(display_vars = exprs(USUBJID, VISIT, PARCAT1, PARAMCD, AVAL, AVALCAT1))

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- derive_var_obs_number(
  adex,
  new_var = ASEQ,
  by_vars = exprs(STUDYID, USUBJID),
  order = exprs(PARCAT1, ASTDT, VISIT, VISITNUM, EXSEQ, PARAMN),
  check_type = "error"
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adex,
  display_vars = exprs(USUBJID, VISIT, PARCAT1, PARAMCD, AVAL, ASTDT, ASEQ)
)

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
adex <- adex %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  )

