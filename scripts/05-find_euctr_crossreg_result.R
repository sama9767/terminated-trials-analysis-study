# This script finds summary results for cross-registered terminated Clinicaltrials.gov trials in the IntoValue dataset.
# Note:
# Cross-registration data is bi-directional and is sourced by:
# 1. Finding additional identifiers in ClincalTrials.gov for terminated ctgov Intovalue trial (https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trn/cross-registrations.rds)
# 2. Finding additional identifiers in EUCTR registry for terminated ctgov Intovalue trial (https://github.com/sama9767/terminated-trials-study/blob/main/scripts/04_find_euctr_crossreg.R)


# get cross-registered data 
iv_terminated_crossreg <- read.csv(here::here("data","processed","intovalue","iv_terminated_crossreg.csv"))

# get intovalue cthist updated dataset
iv_terminated_cthist_updated <- read.csv(here::here("data","processed","intovalue","iv_terminated_cthist_updated.csv"))


# identify trials with no summary result in ctgov and cross-registration in euctr
iv_terminated_filter <- iv_terminated_crossreg |>
  filter(has_summary_results == FALSE & is_crossreg_eudract == TRUE) |>
  select(id,crossreg_euctr_id, is_crossreg_eudract, has_summary_results)


# get function to find result availability in euctr registry
source(here::here("scripts","functions","get_euctr_results.R"))

# get euctr result for  trials with no summary result in ctgov and cross-registration in euctr
euctr_results <- get_euctr_results(iv_terminated_filter$crossreg_euctr_id)


# save cross registered euctr result dataset
#write.csv(euctr_results,file = here::here("data", "processed", "intovalue", "euctr_results.csv"),row.names = FALSE)
euctr_results <- read.csv(here::here("data", "processed", "intovalue", "euctr_results.csv"))

# join the information in intovalue crossreg dataset
iv_terminated_crossreg <- iv_terminated_crossreg |>
  left_join(euctr_results, by = c("crossreg_euctr_id" = "euctr_id")) 

# add an indicator for summary result in euctr registry
iv_terminated_crossreg <- iv_terminated_crossreg |>
  mutate(has_summary_results_euctr = if_else(result == "result found", TRUE, FALSE))

# add this information in main intovalue terminated dataset
iv_terminated_cthist_updated <- iv_terminated_cthist_updated |>
  left_join(iv_terminated_crossreg, by = c("nctid" = "id")) |>
  mutate(has_summary_results = coalesce(iv_terminated_crossreg$has_summary_results_euctr, iv_terminated_cthist_updated$has_summary_result_ctgov)) |>
  select(-has_summary_result_ctgov,-has_summary_results_euctr,-crossreg_euctr_id,-is_crossreg_eudract,-result, -link)