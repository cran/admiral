## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)
library(admiral)
library(admiraldev)

## -----------------------------------------------------------------------------
visits <- c("Baseline", NA, "Screening", "Week 1 Day 7")
visits != "Baseline"

## -----------------------------------------------------------------------------
is.na(visits)

## -----------------------------------------------------------------------------
visits != "Baseline" | is.na(visits)

## -----------------------------------------------------------------------------
mean(c(1, NA, 2))

## -----------------------------------------------------------------------------
mean(c(1, NA, 2), na.rm = TRUE)

## ----eval = TRUE--------------------------------------------------------------
library(rlang)

adae <- data.frame(USUBJID = "XXX-1", AEDECOD = "HEADACHE")

# Return the adae object
adae

# Return an expression
expr(adae)

## ----eval = FALSE-------------------------------------------------------------
#  derive_vars_merged(
#    adsl,
#    dataset_add = ex,
#    filter_add = !is.na(EXENDTM),
#    by_vars = exprs(STUDYID, USUBJID),
#    new_vars = exprs(
#      TRTEDTM = EXENDTM,
#      TRTETMF = EXENTMF,
#      COMPTRT = if_else(!is.na(EXENDTM), "Y", "N")
#    ),
#    order = exprs(EXENDTM),
#    mode = "last"
#  )

## ----eval = TRUE--------------------------------------------------------------
a <- expr(2)
b <- expr(3)

expr(a + b)
# NOT 2 + 3

## ----eval = TRUE--------------------------------------------------------------
expr(!!a + !!b)

## ----eval = TRUE--------------------------------------------------------------
exprs(!!!list(a, b))

## ----eval = TRUE--------------------------------------------------------------
get_admiral_option("subject_keys")

## ----eval = TRUE, error = TRUE------------------------------------------------
adcm <- data.frame(STUDYID = "XXX", USUBJID = "XXX-1", CMTRT = "ASPIRIN")
adcm

# This doesn't work as we are not unquoting the subject keys
adcm %>% select(get_admiral_option("subject_keys"))

# This works because we are unquoting the subject keys
adcm %>% select(!!!get_admiral_option("subject_keys"))

## ----eval = TRUE, echo = TRUE-------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(admiral)

vs <- tribble(
  ~USUBJID, ~VSTESTCD, ~VISIT, ~VSSTRESN, ~VSSTRESU, ~VSDTC,
  "01-1301", "WEIGHT", "SCREENING", 82.1, "kg", "2013-08-29",
  "01-1301", "WEIGHT", "WEEK 2", 81.19, "kg", "2013-09-15",
  "01-1301", "WEIGHT", "WEEK 4", 82.56, "kg", "2013-09-24",
  "01-1302", "BMI", "SCREENING", 20.1, "kg/m2", "2013-08-29",
  "01-1302", "BMI", "WEEK 2", 20.2, "kg/m2", "2013-09-15",
  "01-1302", "BMI", "WEEK 4", 19.9, "kg/m2", "2013-09-24"
)

dm <- tribble(
  ~USUBJID, ~AGE,
  "01-1301", 18
)

## ----eval = TRUE, error = TRUE------------------------------------------------
my_expression <- expr(VSTESTCD == "WEIGHT" & VISIT == "SCREENING")

derive_vars_merged(
  dm,
  dataset_add = select(vs, USUBJID, VSTESTCD, VISIT),
  by_vars = exprs(USUBJID),
  filter_add = my_expression
)

## ----eval = TRUE, error = FALSE-----------------------------------------------
derive_vars_merged(
  dm,
  dataset_add = select(vs, USUBJID, VSTESTCD, VISIT),
  by_vars = exprs(USUBJID),
  filter_add = !!my_expression
)

## ----eval = TRUE, error = TRUE------------------------------------------------
filter_vs_and_merge <- function(my_expression) {
  derive_vars_merged(
    dm,
    dataset_add = select(vs, USUBJID, VSTESTCD, VISIT),
    by_vars = exprs(USUBJID),
    filter_add = !!my_expression
  )
}

# This works
filter_vs_and_merge(expr(VSTESTCD == "WEIGHT" & VISIT == "SCREENING"))

# This fails
filter_vs_and_merge(expr(VSTESTCD == "WEIGHT" & VISIT == "SCREENING" & VSTPT == "PREDOSE"))

