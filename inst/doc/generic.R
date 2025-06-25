## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(admiraldev)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(admiral)
library(tibble)

## ----echo=FALSE---------------------------------------------------------------
library(reactable)

generic_derivations <- tibble::tribble(
  ~Derivation,                      ~Method,       ~What,       ~Source,
  "derive_var_extreme_flag()",      "selection",   "variables", "single",
  "derive_var_joined_exist_flag()", "selection",   "variables", "single",
  "derive_var_merged_ef_msrc()",    "selection",   "variables", "multiple",
  "derive_var_merged_exist_flag()", "selection",   "variables", "single",
  "derive_vars_joined_summary()",   "summary",     "variables", "single",
  "derive_var_merged_summary()",    "summary",     "variables", "single",
  "derive_vars_joined()",           "selection",   "variables", "single",
  "derive_vars_merged()",           "selection",   "variables", "single",
  "derive_vars_extreme_event()",    "selection",   "variables", "multiple",
  "derive_vars_computed()",         "computation", "variables", "single",
  "derive_extreme_event()",         "selection",   "records",   "multiple",
  "derive_extreme_records()",       "selection",   "records",   "single",
  "derive_param_computed()",        "computation", "records",   "single",
  "derive_param_exist_flag()",      "selection",   "records",   "single",
  "derive_summary_records()",       "summary",     "records",   "single"
) %>% dplyr::select(Derivation, What, Source, Method)

reactable(
  generic_derivations,
  columns = list(
    Derivation = colDef(
      minWidth = 200,
      cell = function(value, index) {
        # Render as a link
        url <- paste0(
          "../reference/",
          substr(value, 1, nchar(value) - 2),
          ".html"
        )
        htmltools::tags$a(href = url, as.character(value))
      }
    )
  ),
  defaultSorted = list("What" = "desc", "Source" = "desc", "Method" = "asc"),
  filterable = TRUE,
  resizable = TRUE,
  defaultPageSize = 20
)

## -----------------------------------------------------------------------------
adsl <- tribble(
  ~USUBJID,
  "1",
  "2",
  "3"
)

advs <- tribble(
  ~USUBJID, ~PARAMCD, ~AVISIT,    ~ABLFL, ~AVAL, ~AVALU,
  "1",      "WEIGHT", "BASELINE", "Y",     58.7, "kg",
  "1",      "HEIGHT", "BASELINE", "Y",    169.2, "cm",
  "1",      "WEIGHT", "WEEK 3",   NA,      59.3, "kg",
  "2",      "WEIGHT", "BASELINE", "Y",     72.5, "kg",
  "2",      "WEIGHT", "WEKK 3",   NA,      71.9, "kg",
)

derive_vars_merged(
  adsl,
  dataset_add = advs,
  by_vars = exprs(USUBJID),
  filter_add = PARAMCD == "WEIGHT" & ABLFL == "Y",
  new_vars = exprs(WGTBL = AVAL)
)

## -----------------------------------------------------------------------------
adsl <- tribble(
  ~USUBJID,
  "1",
  "2",
  "3"
)

ex <- tribble(
  ~USUBJID, ~EXSTDY, ~EXDOSE,
  "1",            1,      50,
  "1",            7,      70,
  "1",           14,       0,
  "2",            1,      75,
  "2",            9,      70
)

derive_vars_merged(
  adsl,
  dataset_add = ex,
  by_vars = exprs(USUBJID),
  filter_add = EXDOSE > 0,
  order = exprs(EXSTDY),
  mode = "last",
  new_vars = exprs(TRTEDY = EXSTDY)
)

## -----------------------------------------------------------------------------
adae <- tribble(
  ~USUBJID, ~ASTDY, ~AESEQ,
  "1",           3,      1,
  "1",           3,      2,
  "1",          15,      3
)

ex <- tribble(
  ~USUBJID, ~EXSTDY, ~EXDOSE,
  "1",            1,      50,
  "1",            7,      70,
  "1",           14,       0,
  "2",            1,      75,
  "2",            9,      70
)

derive_vars_joined(
  adae,
  dataset_add = ex,
  by_vars = exprs(USUBJID),
  filter_add = EXDOSE > 0,
  filter_join = EXSTDY <= ASTDY,
  join_type = "all",
  order = exprs(EXSTDY),
  mode = "last",
  new_vars = exprs(LSTDOSDY = EXSTDY, LASTDOS = EXDOSE)
)

## ----echo=FALSE---------------------------------------------------------------
admiral:::get_joined_data(
  adae,
  dataset_add = ex,
  by_vars = exprs(USUBJID),
  filter_add = EXDOSE > 0,
  join_vars = exprs(EXDOSE),
  join_type = "all",
  order = exprs(EXSTDY)
)

## -----------------------------------------------------------------------------
adlb <- tribble(
  ~USUBJID, ~PARAMCD, ~ADY, ~ANRIND,
  "1",      "AST",       1, "HIGH",
  "1",      "AST",       7, "HIGH",
  "1",      "AST",      14, "NORMAL",
  "1",      "ALT",       1, "HIGH",
  "1",      "ALT",       7, "NORMAL",
  "1",      "ALT",      14, "HIGH",
  "2",      "AST",       1, "HIGH",
  "2",      "AST",      15, "HIGH",
  "2",      "AST",      22, "NORMAL",
  "2",      "ALT",       1, "HIGH"
)

derive_var_joined_exist_flag(
  adlb,
  dataset_add = adlb,
  by_vars = exprs(USUBJID, PARAMCD),
  order = exprs(ADY),
  join_vars = exprs(ADY, ANRIND),
  join_type = "after",
  filter_join = ANRIND == "HIGH" & ANRIND.join == "HIGH" & ADY.join > ADY + 10,
  new_var = HICONFFL
)

## ----echo=FALSE---------------------------------------------------------------
admiral:::get_joined_data(
  adlb,
  dataset_add = adlb,
  by_vars = exprs(USUBJID, PARAMCD),
  order = exprs(ADY),
  join_vars = exprs(ADY, ANRIND),
  join_type = "after"
)

## -----------------------------------------------------------------------------
derive_var_joined_exist_flag(
  adlb,
  dataset_add = adlb,
  by_vars = exprs(USUBJID, PARAMCD),
  order = exprs(ADY),
  join_vars = exprs(ADY, ANRIND),
  join_type = "after",
  first_cond_upper = ANRIND.join == "HIGH" & ADY.join > ADY + 10,
  filter_join = ANRIND == "HIGH" & all(ANRIND.join == "HIGH"),
  new_var = HICONFFL
)

## ----echo=FALSE---------------------------------------------------------------
admiral:::get_joined_data(
  adlb,
  dataset_add = adlb,
  by_vars = exprs(USUBJID, PARAMCD),
  order = exprs(ADY),
  join_vars = exprs(ADY, ANRIND),
  join_type = "after",
  first_cond_upper = ANRIND.join == "HIGH" & ADY.join > ADY + 10
)

## -----------------------------------------------------------------------------
advs <- tribble(
  ~USUBJID, ~PARAMCD, ~AVISITN, ~AVAL,
  "1",      "WEIGHT",       NA,  62.1,
  "1",      "WEIGHT",        1,  62.3,
  "1",      "WEIGHT",        2,  62.5,
  "1",      "WEIGHT",        3,  62.4
)

derive_var_extreme_flag(
  advs,
  by_vars = exprs(USUBJID, PARAMCD),
  order = exprs(AVISITN),
  mode = "last",
  new_var = LSTVISFL
)

## -----------------------------------------------------------------------------
derive_var_extreme_flag(
  advs,
  by_vars = exprs(USUBJID, PARAMCD),
  order = exprs(if_else(is.na(AVISITN), -Inf, AVISITN)),
  mode = "last",
  new_var = LSTVISFL
)

## -----------------------------------------------------------------------------
derive_var_extreme_flag(
  advs,
  by_vars = exprs(USUBJID, PARAMCD),
  order = exprs(!is.na(AVISITN), AVISITN),
  mode = "last",
  new_var = LSTVISFL
)

## -----------------------------------------------------------------------------
adex <- tribble(
  ~USUBJID, ~ASTDY, ~AVAL, ~PARAMCD,
  "1",           1,    50, "DOSE",
  "1",           7,    70, "DOSE",
  "1",          14,     0, "DOSE",
  "2",           1,    75, "DOSE",
  "2",           9,    70, "DOSE"
)

derive_summary_records(
  adex,
  dataset_add = adex,
  filter_add = AVAL > 0,
  by_vars = exprs(USUBJID),
  set_values_to = exprs(
    AVAL = mean(AVAL),
    PARAMCD = "AVERAGE DOSE"
  )
)

## -----------------------------------------------------------------------------
adsl <- tribble(
  ~USUBJID,
  "1",
  "2",
  "3"
)

derive_var_merged_summary(
  adsl,
  dataset_add = adex,
  filter_add = AVAL > 0,
  by_vars = exprs(USUBJID),
  new_vars = exprs(
    AVERDOSE = mean(AVAL)
  ),
  missing_values = exprs(AVERDOSE = 0)
)

## -----------------------------------------------------------------------------
adex <- tribble(
  ~USUBJID, ~ADY, ~AVAL,
  "1",         1,    10,
  "1",         8,    20,
  "1",        15,    10,
  "2",         8,     5
)

adae <- tribble(
  ~USUBJID, ~ADY, ~AEDECOD,
  "1",         2, "Fatigue",
  "1",         9, "Influenza",
  "1",        15, "Theft",
  "1",        15, "Fatigue",
  "2",         4, "Parasomnia",
  "3",         2, "Truancy"
)

derive_vars_joined_summary(
  dataset = adae,
  dataset_add = adex,
  by_vars = exprs(USUBJID),
  filter_join = ADY.join <= ADY,
  join_type = "all",
  join_vars = exprs(ADY),
  new_vars = exprs(CUMDOSA = sum(AVAL, na.rm = TRUE)),
  missing_value = exprs(CUMDOSA = 0)
)

## -----------------------------------------------------------------------------
advs <- tribble(
  ~USUBJID, ~AVISIT,    ~PARAMCD, ~AVAL, ~AVALU,
  "1",      "BASELINE", "WEIGHT",  32.6, "kg",
  "1",      "BASELINE", "HEIGHT", 155.4, "cm",
  "1",      "MONTH 6",  "WEIGHT",  33.2, "kg",
  "1",      "MONTH 6",  "HEIGHT", 155.8, "cm",
  "2",      "BASELINE", "WEIGHT",  44.2, "kg",
  "2",      "BASELINE", "HEIGHT", 145.3, "cm",
  "2",      "MONTH 6",  "WEIGHT",  42.0, "kg",
  "2",      "MONTH 6",  "HEIGHT", 146.4, "cm"
)

derive_param_computed(
  advs,
  by_vars = exprs(USUBJID, AVISIT),
  parameters = c("WEIGHT", "HEIGHT"),
  set_values_to = exprs(
    AVAL = AVAL.WEIGHT / (AVAL.HEIGHT / 100)^2,
    PARAMCD = "BMI",
    AVALU = "kg/m^2"
  )
)

## -----------------------------------------------------------------------------
derive_param_computed(
  advs,
  by_vars = exprs(USUBJID, AVISIT),
  parameters = c("WEIGHT", "HEIGHT"),
  set_values_to = exprs(
    AVAL = compute_bmi(weight = AVAL.WEIGHT, height = AVAL.HEIGHT),
    PARAMCD = "BMI",
    AVALU = "kg/m^2"
  )
)

