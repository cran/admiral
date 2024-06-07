## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(admiral)
library(tibble)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)

## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
library(admiraldev)

## -----------------------------------------------------------------------------
windows <- tribble(
  ~AVISIT,    ~AWLO, ~AWHI, ~AVISITN, ~AWTARGET,
  "BASELINE",   -30,     1,        0,         1,
  "WEEK 1",       2,     7,        1,         5,
  "WEEK 2",       8,    15,        2,        11,
  "WEEK 3",      16,    22,        3,        19,
  "WEEK 4",      23,    30,        4,        26
)

## -----------------------------------------------------------------------------
adbds <- tribble(
  ~USUBJID, ~ADY,
  "1",       -33,
  "1",        -2,
  "1",         3,
  "1",        24,
  "2",        NA,
)

adbds1 <- adbds %>%
  derive_vars_joined(
    dataset_add = windows,
    filter_join = AWLO <= ADY & ADY <= AWHI,
    join_type = "all",
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(adbds1)

## ----echo=TRUE----------------------------------------------------------------
ex <- tribble(
  ~STUDYID, ~USUBJID,  ~VISIT,    ~EXTRT,   ~EXSTDTC,
  "xyz",    "1",       "Day 1",   "Drug X", "2022-01-02",
  "xyz",    "1",       "Week 4",  "Drug X", "2022-02-05",
  "xyz",    "1",       "Week 8",  "Drug X", "2022-03-01",
  "xyz",    "1",       "Week 12", "Drug X", "2022-04-03",
  "xyz",    "1",       "Week 16", "Drug Y", "2022-05-03",
  "xyz",    "1",       "Week 20", "Drug Y", "2022-06-02",
  "xyz",    "1",       "Week 24", "Drug Y", "2022-07-01",
  "xyz",    "1",       "Week 28", "Drug Y", "2022-08-04",
  "xyz",    "2",       "Day 1",   "Drug Y", "2023-10-20",
  "xyz",    "2",       "Week 4",  "Drug Y", "2023-11-21",
  "xyz",    "2",       "Week 8",  "Drug Y", "2023-12-19",
  "xyz",    "2",       "Week 12", "Drug Y", "2024-01-19",
  "xyz",    "2",       "Week 16", "Drug X", "2024-02-20",
  "xyz",    "2",       "Week 20", "Drug X", "2024-03-17",
  "xyz",    "2",       "Week 24", "Drug X", "2024-04-22",
  "xyz",    "2",       "Week 28", "Drug X", "2024-05-21"
)

## ----echo=TRUE----------------------------------------------------------------
period_ref <- ex %>%
  # Select visits marking the start of each period
  filter(VISIT %in% c("Day 1", "Week 16")) %>%
  # Create APERIOD, APERSDT, TRTA based on SDTM counterparts
  mutate(
    APERIOD = case_when(
      VISIT == "Day 1" ~ 1,
      VISIT == "Week 16" ~ 2
    ),
    TRTA = EXTRT,
    APERSDT = convert_dtc_to_dt(EXSTDTC)
  ) %>%
  # Create APEREDT based on start date of next period
  arrange(USUBJID, APERSDT) %>%
  group_by(USUBJID) %>%
  mutate(
    APEREDT = lead(APERSDT) - 1 # one day before start of next period
  ) %>%
  # Tidy up
  ungroup() %>%
  select(-starts_with("EX"), -VISIT)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(period_ref)

## ----echo=TRUE----------------------------------------------------------------
adsl <- tribble(
  ~STUDYID,  ~USUBJID,  ~TRTSDT,           ~TRTEDT,           ~EOSDT,
  "xyz",     "1",       ymd("2022-01-02"), ymd("2022-08-04"), ymd("2022-09-10"),
  "xyz",     "2",       ymd("2023-10-20"), ymd("2024-05-21"), ymd("2024-06-30")
)

period_ref <- period_ref %>%
  left_join(adsl, by = c("STUDYID", "USUBJID")) %>%
  mutate(APEREDT = case_when(
    APERIOD == "1" ~ APEREDT,
    APERIOD == "2" ~ EOSDT
  )) %>%
  select(-EOSDT, -TRTSDT, -TRTEDT)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(period_ref)

## ----eval = TRUE--------------------------------------------------------------
adsl1 <- adsl %>%
  mutate(
    PH1SDT = TRTSDT,
    PH1EDT = TRTEDT + 28,
    APHASE1 = "TREATMENT",
    PH2SDT = TRTEDT + 29,
    PH2EDT = EOSDT,
    APHASE2 = "FUP"
  )

phase_ref <- create_period_dataset(
  adsl1,
  new_vars = exprs(PHSDT = PHwSDT, PHEDT = PHwEDT, APHASE = APHASEw)
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(phase_ref)

## ----eval = TRUE--------------------------------------------------------------
adsl2 <- derive_vars_period(
  adsl,
  dataset_ref = period_ref,
  new_vars = exprs(APxxSDT = APERSDT, APxxEDT = APEREDT)
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adsl2,
  display_vars = exprs(STUDYID, USUBJID, AP01SDT, AP01EDT, AP02SDT, AP02EDT)
)

## -----------------------------------------------------------------------------
adae <- tribble(
  ~STUDYID, ~USUBJID, ~ASTDT,
  "xyz",    "1",      "2022-01-31",
  "xyz",    "1",      "2022-05-02",
  "xyz",    "1",      "2022-09-03",
  "xyz",    "1",      "2022-09-09",
  "xyz",    "2",      "2023-12-25",
  "xyz",    "2",      "2024-06-19",
) %>%
  mutate(ASTDT = ymd(ASTDT))

adae1 <- adae %>%
  derive_vars_joined(
    dataset_add = phase_ref,
    by_vars = exprs(STUDYID, USUBJID),
    filter_join = PHSDT <= ASTDT & ASTDT <= PHEDT,
    join_type = "all"
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(adae1)

## -----------------------------------------------------------------------------
adsl <- derive_vars_period(
  adsl,
  dataset_ref = period_ref,
  new_vars = exprs(
    APxxSDT = APERSDT,
    APxxEDT = APEREDT,
    TRTxxA = TRTA
  )
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adsl,
  display_vars = exprs(STUDYID, USUBJID, TRT01A, TRT02A, AP01SDT, AP01EDT, AP02SDT, AP02EDT)
)

## -----------------------------------------------------------------------------
adae <- tribble(
  ~STUDYID, ~USUBJID, ~ASTDT,
  "xyz",    "1",      "2022-01-31",
  "xyz",    "1",      "2022-05-02",
  "xyz",    "1",      "2022-08-24",
  "xyz",    "1",      "2022-09-09",
  "xyz",    "2",      "2023-12-25",
  "xyz",    "2",      "2024-06-07",
) %>%
  mutate(ASTDT = ymd(ASTDT))

adae2 <- adae %>%
  derive_vars_joined(
    dataset_add = period_ref,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(APERIOD, TRTA),
    join_vars = exprs(APERSDT, APEREDT),
    join_type = "all",
    filter_join = APERSDT <= ASTDT & ASTDT <= APEREDT
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(adae2)

## -----------------------------------------------------------------------------
period_ref1 <- adsl %>%
  create_period_dataset(
    new_vars = exprs(APERSDT = APxxSDT, APEREDT = APxxEDT, TRTA = TRTxxA)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(period_ref1)

