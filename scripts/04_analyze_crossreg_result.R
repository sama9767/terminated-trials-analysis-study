# Analyze summary results for cross-registered terminated ClinicalTrials.gov trials in the IntoValue dataset.

library(dplyr)

# Load cross-registered EUCTR data for terminated IntoValue trials
iv_terminated_crossreg <- read_csv(here::here("data", "processed", "intovalue", "iv_terminated_crossreg.csv"))

# Load ClinicalTrials.gov terminated trials data (source from 02-categorize_terminated_trials.R)
terminated_combined <- read.csv(here::here("data","processed","terminated_combined_2.csv"))

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
terminated_combined <- 
  terminated_combined |>
  left_join(iv_terminated_crossreg, by = c("nctid" = "id")) |>
  mutate(has_summary_result_updated = coalesce(has_summary_results_euctr, has_summary_result)) |>
  select(-has_summary_results_euctr, -crossreg_euctr_id, -is_crossreg_eudract, -result, -link, -has_summary_results)

# Arrange column for readability
terminated_combined <- 
  terminated_combined |>
  select(nctid, source, reason_for_termination, reason_category, has_summary_result, has_summary_result_updated,
         start_date, stop_date, trial_days, anticipated_enrollment, actual_enrollment, primary_reason_recoded)

# Get trials for SAE analysis
SAE_analysis_trials <- subset(terminated_combined, 
                          reason_category == "non_scientific_reason" & has_summary_result_updated == TRUE)
print(SAE_analysis_trials)


#write.csv(terminated_combined, file = here::here("data", "processed", "terminated_combined_3.csv"))
#write.csv(iv_terminated_crossreg, file = here::here("data", "processed", "intovalue","iv_terminated_crossreg_updated.csv"))