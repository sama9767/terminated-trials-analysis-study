# Analyze completed trials

library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(rio)
library(terminatedtrialsstudy)

# --- Part 1: Intovalue Data Analysis ----

# Load Intovalue dataset
intovalue_raw <- import("https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trials.rds?raw=true")

# Filter and clean the data (Completed Intovalue trials)
completed_intovalue <- 
  intovalue_raw |>
  filter(
    iv_completion,
    iv_status,
    iv_interventional,
    has_german_umc_lead,
    !(is_dupe & iv_version == 1),
    registry == "ClinicalTrials.gov",
    recruitment_status == "Completed"
  ) |>
  select(id, phase, has_summary_results, has_publication, days_cd_to_publication, days_cd_to_summary) 


# Load historical versions data (see here for details: https://github.com/bgcarlisle/cthist)
cthist_raw <- read_csv(here::here("data", "processed", "2023-12-01-historical-versions.csv"))

# Filter only completed Intovalue trials
cthist_completed_intovalue <- cthist_raw |> filter(nctid %in% completed_intovalue$id)

# Generate characteristics using functions
functions_list <- list(
  get_anticipated_enrollment = terminatedtrialsstudy::get_anticipated_enrollment,
  get_actual_enrollment = terminatedtrialsstudy::get_actual_enrollment,
  has_summary_result = terminatedtrialsstudy::has_summary_result
)


cthist_completed_intovalue <- 
  functions_list |>
  map_dfc(~ .x(cthist_completed_intovalue$nctid, historical_version = TRUE, cthist_completed_intovalue)) |>
  select(nctid...1, anticipated_enrollment, actual_enrollment, has_summary_result) |>
  rename(nctid = nctid...1)

# Merge with completed_intovalue data to get phase and result variables
cthist_completed_intovalue <- 
  left_join(cthist_completed_intovalue, completed_intovalue, by = c("nctid" = "id"))

# Manually assign missing anticipated enrollment values (for trials with 'check missing values warning)
cthist_completed_intovalue <- 
  cthist_completed_intovalue |>
  mutate(
    anticipated_enrollment = case_when(
      nctid == "NCT00160966" ~ 100,
      nctid == "NCT02371434" ~ 9, 
      nctid == "NCT02554318" ~ 129,
      nctid == "NCT01837901" ~ 55,
      TRUE ~ anticipated_enrollment
    ),
    # Ensure integer type after all replacements
    anticipated_enrollment = as.integer(anticipated_enrollment)
  )

# Compute enrollment degree
completed_intovalue_enrol <- cthist_completed_intovalue |>
  filter(
    !is.na(anticipated_enrollment),
    !is.na(actual_enrollment),
    anticipated_enrollment > 0  
  ) |>
  mutate(
    anticipated_enrollment = as.integer(anticipated_enrollment),
    actual_enrollment = as.integer(actual_enrollment)
  )
iv_enrollment_stats  <- degree_of_enrollment(completed_intovalue_enrol, anticipated_column = "anticipated_enrollment", actual_column = "actual_enrollment", trial_column = "nctid")


# Recoding trial phases
cthist_completed_intovalue <- mutate(cthist_completed_intovalue, phase_recoded = case_when(
  phase %in% c("Early Phase 1", "1", "Phase 1") ~ "I",
  phase == "Phase 1/Phase 2" ~ "I-II",
  phase %in% c("2", "Phase 2") ~ "II",
  phase == "Phase 2/Phase 3" ~ "II-III",
  phase %in% c("3", "Phase 3") ~ "III",
  phase %in% c("4", "Phase 4") ~ "IV",
  phase %in% c("0", "Not Applicable") ~ "Not given"
))

# Compute result reporting statistics
summary_results_iv <- round(mean(cthist_completed_intovalue$has_summary_results, na.rm = TRUE) * 100)
publication_iv <- round(mean(cthist_completed_intovalue$has_publication, na.rm = TRUE) * 100)

# Any result reporting (either summary or publication)
cthist_completed_intovalue <- cthist_completed_intovalue |> 
  mutate(days_to_publ = pmin(days_cd_to_publication, days_cd_to_summary, na.rm = TRUE),
         has_publ_or_summary = has_publication | has_summary_results)

any_result_iv <- round(mean(cthist_completed_intovalue$has_publ_or_summary, na.rm = TRUE) * 100)

# Generate table for Intovalue completed trials data
completed_intovalue_table <- furniture::table1(
  cthist_completed_intovalue, 
  phase_recoded, 
  summary_results_iv, 
  publication_iv, 
  any_result_iv, 
  na.rm = FALSE
)

# --- Part 2: Contrast Data Analysis ----

# Load Contrast dataset
contrast_raw <- read.csv(here::here("data", "raw", "contrast", "California-trials_2014-2017_exp_updated.csv"), sep = ";")


# Filter for completed trials
completed_contrast <- contrast_raw |> filter(recruitment_status == "Completed") |> 
  select(id, phase, has_summary_results, has_publication, is_prospective, publication_date, summary_results_date, completion_date)

# Load historical versions data
cthist_raw <- read_csv(here::here("data", "processed", "06_06_2025_historical_versions_updated.csv"))

# Get historical versions for completed contrast trials
cthist_completed_contrast <- cthist_raw |> filter(nctid %in% completed_contrast$id)

# Apply functions to extract characteristics
functions_list <- list(
  get_anticipated_enrollment = terminatedtrialsstudy::get_anticipated_enrollment,
  get_actual_enrollment = terminatedtrialsstudy::get_actual_enrollment,
  has_summary_result = terminatedtrialsstudy::has_summary_result
)

cthist_completed_contrast <- 
  functions_list |>
  map_dfc(~ .x(cthist_completed_contrast$nctid, historical_version = TRUE, cthist_completed_contrast)) |>
  select(nctid...1, anticipated_enrollment, actual_enrollment, has_summary_result) |>
  rename(nctid = nctid...1)

# Manually assign missing anticipated enrollment values (for trials with 'check missing values warning)
cthist_completed_contrast <- 
  cthist_completed_contrast |>
  mutate(
    anticipated_enrollment = case_when(
      # Included trials 
      nctid == "NCT01699789" ~ 1782, 
      nctid == "NCT02262962" ~ 249 ,
      nctid == "NCT02910037" ~ 204, 
      nctid == "NCT02367430" ~ 28, 
      nctid == "NCT02141581" ~ 70, 
      nctid == "NCT00296296" ~ 40,
      nctid == "NCT03040154" ~ 215,
      nctid == "NCT02009046" ~ 2379,
      nctid == "NCT00023595" ~ 2800,
      TRUE ~ anticipated_enrollment  # Keep existing values if none of the conditions match
    )
  )



# Merge with completed_contrast to add phase and result variables
cthist_completed_contrast <- 
  left_join(cthist_completed_contrast, completed_contrast, by = c("nctid" = "id"))

# Compute enrollment degree
completed_contrast_enrol <- cthist_completed_contrast |> filter(!is.na(anticipated_enrollment) & !is.na(actual_enrollment))
ct_enrollment_stats  <- degree_of_enrollment(completed_contrast_enrol, anticipated_column = "anticipated_enrollment", actual_column = "actual_enrollment", trial_column = "nctid")

# Recoding trial phases
cthist_completed_contrast <- mutate(cthist_completed_contrast, phase_recoded = case_when(
  phase %in% c("Early Phase 1", "1", "Phase 1") ~ "I",
  phase == "Phase 1/Phase 2" ~ "I-II",
  phase %in% c("2", "Phase 2") ~ "II",
  phase == "Phase 2/Phase 3" ~ "II-III",
  phase %in% c("3", "Phase 3") ~ "III",
  phase %in% c("4", "Phase 4") ~ "IV",
  phase %in% c("0", "Not Applicable") ~ "Not given"
))

# Compute key statistics
summary_results_ct <- round(mean(cthist_completed_contrast$has_summary_results, na.rm = TRUE) * 100)
publication_ct <- round(mean(cthist_completed_contrast$has_publication, na.rm = TRUE) * 100)


# Calculate time-to variables

# days_cd_to_publication
cthist_completed_contrast <- mutate(cthist_completed_contrast, days_cd_to_publication = 
                               as.integer(ymd_hms(publication_date)-ymd_hms(completion_date)))
cthist_completed_contrast <- mutate(cthist_completed_contrast, days_cd_to_summary = 
                               as.integer(ymd_hms(summary_results_date)-ymd_hms(completion_date)))

# Any result reporting (SR or publication)
cthist_completed_contrast <- 
  cthist_completed_contrast |>
  mutate(days_to_publ = pmin(days_cd_to_publication,   #get minimum of days to pub or to summary result
                             days_cd_to_summary, na.rm = TRUE)) |>
  mutate(has_publ_or_summary = (has_publication)== "TRUE" | (has_summary_results== "TRUE"))


any_result_ct <- mean(cthist_completed_contrast$has_publ_or_summary) * 100


# Generate table for Contrast completed trials data
completed_contrast_table <- furniture::table1(
  cthist_completed_contrast, 
  phase_recoded, 
  summary_results_ct, 
  publication_ct, 
  any_result_ct, 
  na.rm = FALSE
)


# Combine datasets with source column added
completed_combined <- bind_rows(
  mutate(cthist_completed_intovalue, source = "Intovalue") |> 
    select(nctid, anticipated_enrollment, actual_enrollment, has_summary_results,has_publication, phase_recoded,has_publ_or_summary,source),
  
  mutate(cthist_completed_contrast, source = "Contrast") |> 
    select(nctid, anticipated_enrollment, actual_enrollment, has_summary_results,has_publication, phase_recoded,has_publ_or_summary,source))

#write.csv(completed_combined, here::here("data", "processed", "completed_combined.csv"), row.names = FALSE)
