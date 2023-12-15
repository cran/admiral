## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(admiraldev)

## ----message=FALSE------------------------------------------------------------
library(admiral)
library(pharmaversesdtm)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(tibble)

data("lb")

adsl <- admiral_adsl
lb <- convert_blanks_to_na(lb)

## ----echo=FALSE---------------------------------------------------------------
lb <- filter(lb, USUBJID %in% c("01-701-1115", "01-705-1186", "01-705-1349", "01-708-1286", "01-707-1037", "01-716-1024"))

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv4 %>%
  filter(!is.na(TERM)) %>%
  dataset_vignette(
    display_vars = exprs(TERM)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv5 %>%
  filter(!is.na(TERM)) %>%
  dataset_vignette(
    display_vars = exprs(TERM)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_daids %>%
  filter(!is.na(TERM)) %>%
  distinct(TERM) %>%
  dataset_vignette(
    display_vars = exprs(TERM)
  )

## ----eval=TRUE----------------------------------------------------------------
# Look-up tables ----

# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~LBTESTCD, ~PARAMCD,  ~PARAM,                                             ~PARAMN,
  "ALB",     "ALB",     "Albumin (g/L)",                                    1,
  "ALP",     "ALKPH",   "Alkaline Phosphatase (U/L)",                       2,
  "ALT",     "ALT",     "Alanine Aminotransferase (U/L)",                   3,
  "ANISO",   "ANISO",   "Anisocytes",                                       4,
  "AST",     "AST",     "Aspartate Aminotransferase (U/L)",                 5,
  "BASO",    "BASO",    "Basophils (10^9/L)",                               6,
  "BASOLE",  "BASOLE",  "Basophils/Leukocytes (FRACTION)",                  7,
  "BILI",    "BILI",    "Bilirubin (umol/L)",                               8,
  "BUN",     "BUN",     "Blood Urea Nitrogen (mmol/L)",                     9,
  "CA",      "CA",      "Calcium (mmol/L)",                                 10,
  "CHOL",    "CHOLES",  "Cholesterol (mmol/L)",                             11,
  "CK",      "CK",      "Creatinine Kinase (U/L)",                          12,
  "CL",      "CL",      "Chloride (mmol/L)",                                13,
  "COLOR",   "COLOR",   "Color",                                            14,
  "CREAT",   "CREAT",   "Creatinine (umol/L)",                              15,
  "EOS",     "EOS",     "Eosinophils (10^9/L)",                             16,
  "EOSLE",   "EOSLE",   "Eosinophils/Leukocytes (FRACTION)",                17,
  "GGT",     "GGT",     "Gamma Glutamyl Transferase (U/L)",                 18,
  "GLUC",    "GLUC",    "Glucose (mmol/L)",                                 19,
  "HBA1C",   "HBA1C",   "Hemoglobin A1C (1)",                               20,
  "HCT",     "HCT",     "Hematocrit (1)",                                   21,
  "HGB",     "HGB",     "Hemoglobin (mmol/L)",                              22,
  "K",       "POTAS",   "Potassium (mmol/L)",                               23,
  "KETONES", "KETON",   "Ketones",                                          24,
  "LYM",     "LYMPH",   "Lymphocytes (10^9/L)",                             25,
  "LYMLE",   "LYMPHLE", "Lymphocytes/Leukocytes (FRACTION)",                26,
  "MACROCY", "MACROC",  "Macrocytes",                                       27,
  "MCH",     "MCH",     "Ery. Mean Corpuscular Hemoglobin (fmol(Fe))",      28,
  "MCHC",    "MCHC",    "Ery. Mean Corpuscular HGB Concentration (mmol/L)", 29,
  "MCV",     "MCV",     "Ery. Mean Corpuscular Volume (f/L)",               30,
  "MICROCY", "MICROC",  "Microcytes",                                       31,
  "MONO",    "MONO",    "Monocytes (10^9/L)",                               32,
  "MONOLE",  "MONOLE",  "Monocytes/Leukocytes (FRACTION)",                  33,
  "PH",      "PH",      "pH",                                               34,
  "PHOS",    "PHOS",    "Phosphate (mmol/L)",                               35,
  "PLAT",    "PLAT",    "Platelet (10^9/L)",                                36,
  "POIKILO", "POIKIL",  "Poikilocytes",                                     37,
  "POLYCHR", "POLYCH",  "Polychromasia",                                    38,
  "PROT",    "PROT",    "Protein (g/L)",                                    39,
  "RBC",     "RBC",     "Erythrocytes (TI/L)",                              40,
  "SODIUM",  "SODIUM",  "Sodium (mmol/L)",                                  41,
  "SPGRAV",  "SPGRAV",  "Specific Gravity",                                 42,
  "TSH",     "TSH",     "Thyrotropin (mU/L)",                               43,
  "URATE",   "URATE",   "Urate (umol/L)",                                   44,
  "UROBIL",  "UROBIL",  "Urobilinogen",                                     45,
  "VITB12",  "VITB12",  "Vitamin B12 (pmol/L)",                             46,
  "WBC",     "WBC",     "Leukocytes (10^9/L)",                              47
)

adlb <- lb %>%
  ## Add PARAMCD PARAM and PARAMN - from LOOK-UP table
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM, PARAMN),
    by_vars = exprs(LBTESTCD)
  ) %>%
  ## Calculate PARCAT1 AVAL AVALC ANRLO ANRHI
  ## Dummy the values for BASE
  mutate(
    PARCAT1 = LBCAT,
    AVAL = LBSTRESN,
    AVALC = LBSTRESC,
    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI,
    BASE = AVAL - 10
  )

## ----eval=TRUE----------------------------------------------------------------
# Assign ATOXDSCL and ATOXDSCH to hold lab grading terms
# ATOXDSCL and ATOXDSCH hold terms defined by NCI-CTCAEv4.
grade_lookup <- tibble::tribble(
  ~PARAMCD, ~ATOXDSCL,                    ~ATOXDSCH,
  "ALB",    "Hypoalbuminemia",            NA_character_,
  "ALKPH",  NA_character_,                "Alkaline phosphatase increased",
  "ALT",    NA_character_,                "Alanine aminotransferase increased",
  "AST",    NA_character_,                "Aspartate aminotransferase increased",
  "BILI",   NA_character_,                "Blood bilirubin increased",
  "CA",     "Hypocalcemia",               "Hypercalcemia",
  "CHOLES", NA_character_,                "Cholesterol high",
  "CK",     NA_character_,                "CPK increased",
  "CREAT",  NA_character_,                "Creatinine increased",
  "GGT",    NA_character_,                "GGT increased",
  "GLUC",   "Hypoglycemia",               "Hyperglycemia",
  "HGB",    "Anemia",                     "Hemoglobin increased",
  "POTAS",  "Hypokalemia",                "Hyperkalemia",
  "LYMPH",  "CD4 lymphocytes decreased",  NA_character_,
  "PHOS",   "Hypophosphatemia",           NA_character_,
  "PLAT",   "Platelet count decreased",   NA_character_,
  "SODIUM", "Hyponatremia",               "Hypernatremia",
  "WBC",    "White blood cell decreased", "Leukocytosis",
)

adlb <- adlb %>%
  derive_vars_merged(
    dataset_add = grade_lookup,
    by_vars = exprs(PARAMCD),
  )

## ----eval=TRUE----------------------------------------------------------------
adlb <- adlb %>%
  derive_var_atoxgr_dir(
    new_var = ATOXGRL,
    tox_description_var = ATOXDSCL,
    meta_criteria = atoxgr_criteria_ctcv4,
    criteria_direction = "L",
    get_unit_expr = extract_unit(PARAM)
  ) %>%
  derive_var_atoxgr_dir(
    new_var = ATOXGRH,
    tox_description_var = ATOXDSCH,
    meta_criteria = atoxgr_criteria_ctcv4,
    criteria_direction = "H",
    get_unit_expr = extract_unit(PARAM)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv4 %>%
  filter(!is.na(SI_UNIT_CHECK)) %>%
  dataset_vignette(
    display_vars = exprs(TERM, SI_UNIT_CHECK),
  )

## ----eval=TRUE----------------------------------------------------------------
adlb <- adlb %>%
  derive_var_atoxgr()

## ----eval=TRUE, echo=FALSE----------------------------------------------------
adlb %>%
  filter((ATOXGRL == "1") | (ATOXGRH == "1")) %>%
  dataset_vignette(
    display_vars = exprs(ATOXDSCL, ATOXDSCH, ATOXGRL, ATOXGRH, ATOXGR)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv4 %>%
  filter(str_detect(TERM, "calcemia")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv4 %>%
  filter(str_detect(TERM, "glycemia")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv4 %>%
  filter(str_detect(TERM, "INR")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, Grade_1)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv4 %>%
  filter(str_detect(TERM, "INR")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv4 %>%
  filter(str_detect(TERM, "Hyperuricemia")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, Grade_1, Grade_3, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv4 %>%
  filter(str_detect(TERM, "Hypokalemia")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, Grade_1, Grade_2, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv5 %>%
  filter(str_detect(TERM, "calcemia")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv5 %>%
  filter(str_detect(TERM, "INR")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, Grade_1)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv5 %>%
  filter(str_detect(TERM, "INR")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv5 %>%
  filter(str_detect(TERM, "Lipase") | str_detect(TERM, "amylase")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, Grade_2, Grade_3, Grade_4)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv5 %>%
  filter(str_detect(TERM, "INR") | str_detect(TERM, "amylase")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv5 %>%
  filter(str_detect(TERM, "Hyperuricemia")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, Grade_1, Grade_3, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_ctcv5 %>%
  filter(str_detect(TERM, "Hypokalemia") | str_detect(TERM, "Hyponatremia")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, Grade_1, Grade_2, Grade_3, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_daids %>%
  filter(str_detect(TERM, "Cholesterol")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, FILTER)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_daids %>%
  filter(str_detect(COMMENT, "No criteria given")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, FILTER, GRADE_CRITERIA_CODE)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_daids %>%
  filter(TERM %in% c("INR, High", "PT, High", "PTT, High")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_daids %>%
  filter(str_detect(COMMENT, "HIV infected")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_daids %>%
  filter(str_detect(COMMENT, "lifethreatening")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_daids %>%
  filter(str_detect(TERM, "Lactate")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, Grade_1, Grade_1)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_daids %>%
  filter(str_detect(TERM, "Lactate")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, COMMENT)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
atoxgr_criteria_daids %>%
  filter(str_detect(COMMENT, "conver")) %>%
  dataset_vignette(
    display_vars = exprs(TERM, FILTER, COMMENT)
  )

