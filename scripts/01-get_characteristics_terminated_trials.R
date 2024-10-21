# Analyzing terminated trials using two datasets from German and Californian UMCs

#remotes::install_github("sama9767/terminated-trials-study", force = TRUE)
library(terminatedtrialsstudy)
library(dplyr)
library(readr)
library(aactr)
library(purrr)

# Prepare trials for characterizing


# Get data -----------------

intovalue_raw <- rio::import("https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trials.rds?raw=true")
contrast_raw <- read.csv(here::here("data","raw","contrast", "California-trials_2014-2017_exp.csv"), sep = ";")

# Get historical version of included trial data
cthist_raw <- read_csv(here::here("data", "processed", "2023-12-01-historical-versions.csv"))


# Prepare----------------------------------

# Filter for terminated trials with actual enrollment > 0
cthist_terminated_id <- cthist_raw |>
  group_by(nctid) |>
  summarise(
      final_overall_status = last(overall_status),
      final_enrollment_type = last(enrolment_type),
      final_enrollment = last(enrolment)
  ) |>
    filter(
        final_overall_status == "TERMINATED" &
        final_enrollment_type == "ACTUAL" &
        final_enrollment > 0
    ) |>
  ungroup()

cthist_terminated <- 
  cthist_raw |>
  filter(nctid %in% cthist_terminated_id$nctid)
  

# Analyse-------------------------------

# Apply functions to generate characteristics for terminated trials
functions_list <- list(
  get_why_stopped = terminatedtrialsstudy::get_why_stopped,
  get_anticipated_enrollment = terminatedtrialsstudy::get_anticipated_enrollment,
  get_actual_enrollment = terminatedtrialsstudy::get_actual_enrollment,
  has_summary_result = terminatedtrialsstudy::has_summary_result,
  get_start_date = terminatedtrialsstudy::get_start_date,
  get_stop_date = terminatedtrialsstudy::get_stop_date
)

cthist_terminated <- 
  functions_list |>
  map_dfc(~ .x(cthist_terminated$nctid, historical_version = TRUE, cthist_terminated)) |>
  select(nctid...1, reason_for_termination, anticipated_enrollment,actual_enrollment, has_summary_result, start_date, stop_date ) |>
  rename(nctid = nctid...1)


# Merge with intovalue and contrast data for source tagging
cthist_terminated <- 
  cthist_terminated |>
  mutate(source = case_when(
    nctid %in% intovalue_raw$id ~ "intovalue", # If nctid is only in intovalue_raw
    nctid %in% contrast_raw$id ~ "contrast", # If nctid is only in contrast_raw
    TRUE ~ NA_character_ # If nctid is in neither
  ))

 
# Manually assign missing anticipated enrollment values (for trials with 'check missing values warning)
cthist_terminated <- 
  cthist_terminated |>
  mutate(
    anticipated_enrollment = case_when(
      nctid == "NCT00150878" ~ 172,
      nctid == "NCT01030666" ~ 61,
      nctid == "NCT01297712" ~ 1069,
      nctid == "NCT01215266" ~ 98,
      nctid == "NCT00405873" ~ 73,
      nctid == "NCT01071135" ~ 5,
      nctid == "NCT01199133" ~ 471,
      nctid == "NCT02188212" ~ 53,
      nctid == "NCT02585752" ~ 20,
      nctid == "NCT02595840" ~ 4,
      nctid == "NCT02818959" ~ 7,
      nctid == "NCT02891863" ~ 9,
      nctid == "NCT02634606" ~ 13,
      nctid == "NCT03023709" ~ 3,
      nctid == "NCT03028987" ~ 12,
      nctid == "NCT03115385" ~ 22,
      nctid == "NCT02967380" ~ 14,
      TRUE ~ anticipated_enrollment  # Keep existing values if none of the conditions match
    )
  )


# Calculate degree of enrollment and trial duration
cthist_terminated <- terminatedtrialsstudy::degree_of_enrollment(cthist_terminated, anticipated_column = "anticipated_enrollment" , actual_column = "actual_enrollment", round_off = 2)
cthist_terminated <- duration_of_trial(cthist_terminated, "start_date", "stop_date")



# Get therapeutic foci of included trials
#cthist_terminated <- TrialFociMapper::get_foci_ctgov(cthist_terminated$nctid, username = "username", password = "password")
#cthist_terminated <- TrialFociMapper::assign_therapeutic_focus(data = cthist_terminated, nctid_col = nct_id, mesh_heading_cols = 'trial_foci_table_list')
cthist_terminated_foci <- readRDS(here::here("data", "processed", "2024-09-19-cthist_terminated_foci.rds"))

# Merge the therapeutic focus data with cthist_terminated dataset
cthist_terminated <- 
  cthist_terminated |>
  left_join(cthist_terminated_foci, by = c("nctid" ="nct_id")) 
colnames(cthist_terminated)

# Get statistics -----

# Summary result by source
cthist_terminated |>
  count(has_summary_result, source)

# Calculate the maximum and minimum trial_days for each source
summary_trial_days <- 
  cthist_terminated |>
  group_by(source) |>  # Group by the 'source' column
  summarise(
    max_trial_days = max(trial_days, na.rm = TRUE),  # Maximum value of trial_days for each source
    min_trial_days = min(trial_days, na.rm = TRUE),   # Minimum value of trial_days for each source
    total_trial_days = sum(trial_days, na.rm = TRUE) # Total value of trial_days
    )

# Summarize the median and IQR for anticipated enrollment by source
# Summarize the median, IQR, and specific percentiles for anticipated enrollment by source
summary_anticipated_enrollment <- 
  cthist_terminated |>
  group_by(source) |>  # Group by the 'source' column
  summarise(
    median_anticipated_enrollment = median(anticipated_enrollment, na.rm = TRUE), # Median of anticipated enrollment
    IQR_anticipated_enrollment = IQR(anticipated_enrollment, na.rm = TRUE), # IQR of anticipated enrollment
    Q1_anticipated_enrollment = quantile(anticipated_enrollment, 0.25, na.rm = TRUE), # 25th percentile (Q1)
    Q3_anticipated_enrollment = quantile(anticipated_enrollment, 0.75, na.rm = TRUE), # 75th percentile (Q3)
    max_anticipated_enrollment = quantile(anticipated_enrollment, 1, na.rm = TRUE), # 100th percentile (max)
    .groups = 'drop'  # Drop the grouping structure in the output
  )


# Summarize the median, IQR, and specific percentiles for anticipated enrollment by source
summary_actual_enrollment <- 
  cthist_terminated |>
  group_by(source) |>  # Group by the 'source' column
  summarise(
    median_actual_enrollment = median(actual_enrollment, na.rm = TRUE), # Median of anticipated enrollment
    IQR_actual_enrollment = IQR(actual_enrollment, na.rm = TRUE), # IQR of anticipated enrollment
    Q1_actual_enrollment = quantile(actual_enrollment, 0.25, na.rm = TRUE), # 25th percentile (Q1)
    Q3_actual_enrollment = quantile(actual_enrollment, 0.75, na.rm = TRUE), # 75th percentile (Q3)
    max_actual_enrollment = quantile(actual_enrollment, 1, na.rm = TRUE), # 100th percentile (max)
    .groups = 'drop'  # Drop the grouping structure in the output
  )


saveRDS(cthist_terminated, file = here::here("data", "processed", "cthist_terminated_1.rds"))
