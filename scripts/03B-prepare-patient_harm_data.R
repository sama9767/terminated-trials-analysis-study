# This script identifies summary results for cross-registered terminated ClinicalTrials.gov trials in the IntoValue dataset.
# Cross-registration data includes:
# 1. Finding additional identifiers in ClinicalTrials.gov for terminated ctgov Intovalue trials
# 2. Finding additional identifiers in EUCTR registry for terminated ctgov Intovalue trials

library(dplyr)

# Load cross-registered data
iv_terminated_crossreg <- read.csv(here::here("data", "processed", "intovalue", "iv_terminated_crossreg.csv"))

# Load the updated intovalue dataset
iv_terminated_cthist_updated <- read.csv(here::here("data", "processed", "intovalue", "iv_terminated.csv"))

# Filter trials that:
# - Do not have summary results in ClinicalTrials.gov
# - Are cross-registered in the EUCTR registry
iv_terminated_filter <- 
  iv_terminated_crossreg |>
  filter(has_summary_results == FALSE & is_crossreg_eudract == TRUE) |>
  select(id, crossreg_euctr_id, is_crossreg_eudract, has_summary_results)

# Source the function to check result availability in the EUCTR registry
source(here::here("scripts", "functions", "get_euctr_results.R"))

# Retrieve EUCTR results for trials with no summary results in ClinicalTrials.gov and cross-registration in EUCTR
#euctr_results <- get_euctr_results(iv_terminated_filter$crossreg_euctr_id)

# Save the cross-registered EUCTR results dataset
# write.csv(euctr_results, file = here::here("data", "processed", "intovalue", "euctr_results.csv"), row.names = FALSE)
euctr_results <- read.csv(here::here("data", "processed", "intovalue", "euctr_results.csv"))

# Join the EUCTR result information with the intovalue cross-registration dataset
iv_terminated_crossreg <- 
  iv_terminated_crossreg |>
  left_join(euctr_results, by = c("crossreg_euctr_id" = "euctr_id"))

# Add a flag to indicate summary results availability in the EUCTR registry
iv_terminated_crossreg <- 
  iv_terminated_crossreg |>
  mutate(has_summary_results_euctr = if_else(result == "result found", TRUE, FALSE))

# Add this EUCTR result information to the main intovalue terminated dataset
iv_terminated_cthist_updated <- 
  iv_terminated_cthist_updated |>
  left_join(iv_terminated_crossreg, by = c("nctid" = "id")) |>
  mutate(has_summary_results = coalesce(has_summary_results_euctr, has_summary_result)) |>
  select(-has_summary_results_euctr, -crossreg_euctr_id, -is_crossreg_eudract, -result, -link)

# Save the updated intovalue terminated dataset
#write.csv(iv_terminated_cthist_updated, file = here::here("data", "processed", "intovalue", "iv_terminated_cthist_updated_2.csv"), row.names = FALSE)

# Filter trials that:
# - Were terminated due to non-scientific reasons
# - Do not have summary results in ClinicalTrials.gov
# - Are cross-registered in the EUCTR registry
# - Have summary results available in EUCTR
trials <- iv_terminated_crossreg |>
  left_join(extraction_form, by = c("id" = "nctid"))

trials_updated <- 
  trials |>
  filter(reason_category == "non_scientific_reason" & 
           has_summary_results == FALSE & 
           is_crossreg_eudract == TRUE & 
           result == "result found")

# Save the updated trials dataset
#write.csv(trials_updated, file = here::here("trials_updated.csv"))
