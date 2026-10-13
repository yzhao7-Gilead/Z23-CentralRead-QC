rm(list = ls())

# EFS per central read QC program
library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(writexl)

# -----------------------------------------------------------------------------#
# 1. Data preparation: read all source datasets before deriving any datasets
# -----------------------------------------------------------------------------#
raw_data_dir <- "/fsxn_PrisonersBay/clin_data/KTE-C19/KT-US-484-0136/analysis/live_scrambled/data/raw_cdms"
adam_data_dir <- "/fsxn_PrisonersBay/stat_prog/KTE-C19/KT-US-484-0136/analysis/PA_dryrun/data/adam"



# read raw data
ixrs <- read_sas(file.path(raw_data_dir, "dsrd.sas7bdat"))
cl_rs <- read_sas(file.path(raw_data_dir, "cl_rs.sas7bdat")) # central read
death <- read_sas(file.path(raw_data_dir, "death.sas7bdat"))
osrth <- read_sas(file.path(raw_data_dir, "osrth.sas7bdat")) # subsequent radiotherapy
sprth <- read_sas(file.path(raw_data_dir, "sprth.sas7bdat")) # subsequent transplant
stnhl <- read_sas(file.path(raw_data_dir, "stnhl.sas7bdat")) # subsequent anti-cancer therapy
rs <- read_sas(file.path(raw_data_dir, "rs.sas7bdat")) # overall disease response
bmaba <- read_sas(file.path(raw_data_dir, "bmaba.sas7bdat")) # bone marrow
tissue <- read_sas(file.path(raw_data_dir, "tissue.sas7bdat")) # tissue biopsy
eos <- read_sas(file.path(raw_data_dir, "eos.sas7bdat")) # end of study
ptxds <- read_sas(file.path(raw_data_dir, "ptxds.sas7bdat")) # treatment disposition



# read addam data
adsl <- read_sas(file.path(adam_data_dir, "adsl.sas7bdat"))
adtte <- read_sas(file.path(adam_data_dir, "adtte.sas7bdat"))



# -----------------------------------------------------------------------------#
# 2. Prepre central read qc dataset
# -----------------------------------------------------------------------------#

#------------------- IxRS patient arm, randomization date ---------------------#
central_qc <- ixrs %>% 
  select(Subject, DSSTDAT) %>% 
  rename(RAND_DATE = DSSTDAT)

#------------------- current study status ---------------------#
temp_eos <- eos %>% 
  select(Subject, DSSTDAT, DSTERM) %>% 
  rename(EOS_DATE = DSSTDAT,
         EOS_REASON = DSTERM)
# check missing values
temp_eos %>% 
  filter(
    is.na(EOS_DATE) | is.na(EOS_REASON)
  ) 
central_qc <- central_qc %>% 
  left_join(temp_eos)

#------------------- current treatment status ---------------------#
temp_ptxds <- ptxds %>% 
  select(Subject, DSYN, DSSTDAT, DSSPE) %>% 
  filter(DSYN == "Yes") %>% 
  rename(EOT_DATE = DSSTDAT,
         EOT_REASON = DSSPE) %>% 
  select(-DSYN)
# check missing values and duplicate
temp_ptxds %>% 
  filter(
    is.na(EOT_DATE) | is.na(EOT_REASON)
  )
temp_ptxds %>% 
  filter(
    duplicated(Subject)
  )
central_qc <- central_qc %>% 
  left_join(temp_ptxds)

#------------------- response by investigator ---------------------#
last_assess <- rs %>%
  group_by(Subject) %>%
  slice_max(OVRLDAT, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    Subject,
    LAST_ASSESS_DATE_INV = OVRLDAT,
    LAST_ASSESS_RESP_INV = OVRLRESP
  )

first_cr_pd <- rs %>%
  group_by(Subject) %>%
  summarise(
    FIRST_CR_DATE_INV = if (any(OVRLRESP == "Complete Response"))
      min(OVRLDAT[OVRLRESP == "Complete Response"])
    else as.Date(NA),
    FIRST_PD_DATE_INV = if (any(OVRLRESP == "Progressive Disease"))
      min(OVRLDAT[OVRLRESP == "Progressive Disease"])
    else as.Date(NA),
    .groups = "drop"
  )

rs_summary <- first_cr_pd %>%
  left_join(last_assess, by = "Subject")

central_qc <- central_qc %>% 
  left_join(rs_summary)


#------------------- response by central read ---------------------#
last_assess_cl <- cl_rs %>%
  mutate(RSDTN = as.Date(RSDTN)) %>% 
  rename(Subject = USUBJID) %>% 
  filter(RSEVALID == "ONCOLOGIST",
         RSTEST == "Overall Response") %>% 
  group_by(Subject) %>%
  slice_max(RSDTN, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    Subject,
    LAST_ASSESS_DATE_CEN = RSDTN,
    LAST_ASSESS_RESP_CEN = RSORRES
  )

first_cr_pd_cl <- cl_rs %>%
  mutate(RSDTN = as.Date(RSDTN)) %>% 
  rename(Subject = USUBJID) %>% 
  filter(RSEVALID == "ONCOLOGIST",
         RSTEST == "Overall Response") %>% 
  group_by(Subject) %>%
  summarise(
    FIRST_CR_DATE_CEN = if (any(RSORRES == "CR"))
      min(RSDTN[RSORRES == "CR"])
    else as.Date(NA),
    FIRST_PD_DATE_CEN = if (any(RSORRES == "PD"))
      min(RSDTN[RSORRES == "PD"])
    else as.Date(NA),
    .groups = "drop"
  )

rs_summary_cl <- first_cr_pd_cl %>%
  left_join(last_assess_cl, by = "Subject")

central_qc <- central_qc %>% 
  left_join(rs_summary_cl)

#------------------- flag patients ---------------------#
central_qc <- central_qc %>% 
  mutate(
    # 1. patient progressed per INV but CEN is missing
    FLAG_INV_PD_NO_CEN = if_else(
      !is.na(FIRST_PD_DATE_INV) & is.na(LAST_ASSESS_DATE_CEN), "Y", ""
    ),
    # 2. patient discon study but CEN is missing
    FLAG_DISCON_NO_CEN = if_else(
      !is.na(EOS_REASON) & is.na(LAST_ASSESS_DATE_CEN), "Y", ""
    ),
    # unify date format
    across(all_of(c("EOS_DATE", "EOT_DATE", "FIRST_CR_DATE_INV", "FIRST_PD_DATE_INV",
                  "LAST_ASSESS_DATE_INV")),
           as.Date)
  ) 
  

file_name = paste0("./central_read_missing_", Sys.Date(), ".xlsx")
write_xlsx(central_qc, 
           file_name)

# -----------------------------------------------------------------------------#
# 3. Prepare central read overdue visit dataset                                 
# -----------------------------------------------------------------------------#

visit_schedule <- tibble(
  VISIT = c("D50", "D100", "M6", "M9", "M12", "M18", "M24", "M30", "M36", "M42", 
            "M48", "M54", "M60"),
  TARGET_DAY = c(50, 100, 180, 270, 365, 540, 730, 900, 1095, 1260, 1460, 1620, 1825),
  WINDOW_LOW = c(-7, -14, -14, -28, -28, -28, -28, -28, -28, -28, -28, -28, -28),
  WINDOW_HIGH = c(21, 14, 14, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28)
)

visit_plan <- central_qc %>%
  select(Subject, RAND_DATE, EOS_DATE, EOS_REASON, LAST_ASSESS_DATE_INV, LAST_ASSESS_RESP_INV) %>% 
  crossing(visit_schedule) %>%
  mutate(RAND_DATE = as.Date(RAND_DATE)) %>% 
  mutate(
    SNAPSHOT_DATE = Sys.Date(),
    TARGET_DATE = RAND_DATE + TARGET_DAY - 1,
    WINDOW_START = TARGET_DATE + WINDOW_LOW,
    WINDOW_END = TARGET_DATE + WINDOW_HIGH
  )

next_visit <- visit_plan %>%
  filter(
    is.na(LAST_ASSESS_DATE_INV) | WINDOW_START > LAST_ASSESS_DATE_INV
  ) %>%
  group_by(Subject) %>%
  slice_min(WINDOW_START, n = 1, with_ties = FALSE) %>%
  ungroup()


# determine if a patient don't have an overdue disease assessment
next_visit <- next_visit %>% 
  mutate(
    OVERDUE_VISIT = "disease assessment appear to be overdued"
  ) %>% 
  mutate(
    OVERDUE_VISIT = case_when(
      !is.na(EOS_DATE) ~ "",
      !is.na(LAST_ASSESS_DATE_INV) & (LAST_ASSESS_DATE_INV >= WINDOW_START & LAST_ASSESS_DATE_INV <= WINDOW_START) ~ "",
      WINDOW_START >= LAST_ASSESS_DATE_INV ~ "",
      TRUE ~ OVERDUE_VISIT
    )
  )

file_name = paste0("./disease_assess_overdue_", Sys.Date(), ".xlsx")
write_xlsx(next_visit, 
           file_name)
