# This script identifies summary results for cross-registered terminated ClinicalTrials.gov trials in the IntoValue dataset.
# It checks for summary results in either ClinicalTrials.gov or the EUCTR registry for terminated trials.

# PART 1: Identifying Summary Results for Cross-Registered Terminated Trials

library(dplyr)

# Load cross-registered EUCTR data for terminated IntoValue trials
iv_terminated_crossreg <- read_csv(here::here("data", "processed", "intovalue", "iv_terminated_crossreg.csv"))

# Load ClinicalTrials.gov terminated trials data (source from 02-categorize_terminated_trials.R)
cthist_terminated <- readRDS(here::here("data","processed","cthist_terminated_2.rds"))

# Load EUCTR summary results (previously retrieved using a custom function)
#euctr_results <- terminatedtrialsstudy::get_euctr_results(iv_terminated_filter$crossreg_euctr_id)
#write.csv(euctr_results, file = here::here("data", "processed", "intovalue", "euctr_results.csv"), row.names = FALSE)
euctr_results <- read_csv(here::here("data", "processed", "intovalue", "euctr_results.csv"))

# Join EUCTR results with cross-registered trials data
iv_terminated_crossreg <- 
  iv_terminated_crossreg |>
  left_join(euctr_results, by = c("crossreg_euctr_id" = "euctr_id")) |>
  mutate(has_summary_results_euctr = if_else(result == "result found", TRUE, FALSE))

# Add updated summary result information (CTGOV or EUCTR) to the terminated dataset
cthist_terminated <- 
  cthist_terminated |>
  left_join(iv_terminated_crossreg, by = c("nctid" = "id")) |>
  mutate(has_summary_result_updated = coalesce(has_summary_results_euctr, has_summary_result)) |>
  select(-has_summary_results_euctr, -crossreg_euctr_id, -is_crossreg_eudract, -result, -link, -has_summary_results)

# Arrange column for readability
cthist_terminated <- 
  cthist_terminated |>
  select(nctid, source, reason_for_termination, reason_category, has_summary_result, has_summary_result_updated,
         start_date, stop_date, trial_days, anticipated_enrollment, actual_enrollment, enrollment_percentage,
         trial_foci_table_list, therapeutic_focus, everything())  # 'everything()' keeps other columns intact


# Save the updated terminated trials dataset
saveRDS(cthist_terminated, file = here::here("data", "processed","cthist_terminated_3_final.rds"))


# PART 2: Filter Trials for Patient Harm Analysis

# Load extraction form with categorized reasons for termination
extraction_form_updated <- read_csv(here::here("data", "processed", "terminated_reason_categorized.csv")) |>
  select (-...1)

# Join cross-registration data with reason for termination
patient_harm <- iv_terminated_crossreg |>
  left_join(extraction_form_updated, by = c("id" = "nctid"))

# Filter trials:
# - Terminated due to non-scientific reasons
# - No summary results in ClinicalTrials.gov
# - Cross-registered in EUCTR with summary results
patient_harm_check <- 
  patient_harm |>
  filter(reason_category == "non_scientific_reason" & 
           has_summary_results == FALSE & 
           is_crossreg_eudract == TRUE & 
           result == "result found")

# Save the filtered dataset for patient harm analysis
# write.csv(patient_harm_check, file = here::here("data", "processed", "patient_harm_check.csv"))
