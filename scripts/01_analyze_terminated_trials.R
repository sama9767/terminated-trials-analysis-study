# Analyze terminated trials

#remotes::install_github("sama9767/terminated-trials-study", force = TRUE)
library(terminatedtrialsstudy)
library(dplyr)
library(readr)
library(aactr)
library(purrr)
library(lubridate)


# Get data -----------------

intovalue_raw <- rio::import("https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trials.rds?raw=true")
contrast_raw <- read.csv(here::here("data","raw","contrast", "California-trials_2014-2017_exp_updated.csv"), sep = ";")

# Get historical version of included trial data
cthist_raw <- read_csv(here::here("data", "processed", "2023-12-01-historical-versions.csv"))

# Filter for terminated trials with actual enrollment > 0
terminated_cthist_id <- cthist_raw |>
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

terminated_cthist <- 
  cthist_raw |>
  filter(nctid %in% terminated_cthist_id$nctid) |>
  mutate(source = case_when(
    nctid %in% intovalue_raw$id ~ "intovalue", # If nctid is only in intovalue_raw
    nctid %in% contrast_raw$id ~ "contrast", # If nctid is only in contrast_raw
    TRUE ~ NA_character_ # If nctid is in neither
  ))

# --- Part 1: Intovalue Data Analysis ----  
# Read data for IntoValue terminated trials
terminated_intovalue <- terminated_cthist |>
  filter(source == "intovalue")

# Apply functions to generate characteristics for terminated trials
functions_list <- list(
  get_why_stopped = get_why_stopped,
  get_anticipated_enrollment = get_anticipated_enrollment,
  get_actual_enrollment = get_actual_enrollment,
  has_summary_result = has_summary_result,
  get_start_date = get_start_date,
  get_stop_date = get_stop_date
)

terminated_intovalue <- 
  functions_list |>
  map_dfc(~ .x(terminated_intovalue$nctid, historical_version = TRUE, terminated_intovalue)) |>
  select(nctid...1, reason_for_termination, anticipated_enrollment,actual_enrollment, has_summary_result, start_date, stop_date ) |>
  rename(nctid = nctid...1)


# Manually assign missing anticipated enrollment values (for trials with 'check missing values warning)
terminated_intovalue <- 
  terminated_intovalue |>
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
terminated_intovalue <- duration_of_trial(terminated_intovalue, "start_date", "stop_date")

trial_days_stats <- 
  terminated_intovalue |>
  summarise(
    max_trial_days = max(trial_days, na.rm = TRUE),  
    min_trial_days = min(trial_days, na.rm = TRUE),   
    total_trial_days = sum(trial_days, na.rm = TRUE) 
  )

enrollment_stats <- degree_of_enrollment(terminated_intovalue, anticipated_column =  "anticipated_enrollment", actual_column =  "actual_enrollment")


# Get other characteristics from original Intovalue data
terminated_intovalue <- 
  terminated_intovalue |>
  left_join(intovalue_raw |> 
              select(id, phase, allocation, masking, has_publication, publication_date, days_cd_to_publication, days_cd_to_summary, is_prospective), 
            by = c("nctid" = "id")) |>
  distinct(nctid, .keep_all = TRUE)

# Recode variables
terminated_intovalue <- 
  mutate(terminated_intovalue, phase_recoded_iv = case_when(
    phase == "Early Phase 1" | phase == "1" | phase == "Phase 1" ~ "I",
    phase == "Phase 1/Phase 2" ~ "I-II",
    phase == "2" | phase == "Phase 2" ~ "II",
    phase == "Phase 2/Phase 3" ~ "II-III",
    phase == "3" | phase == "Phase 3" ~ "III",
    phase == "4" | phase == "Phase 4" ~ "IV",
    phase == "0" | phase == "Not Applicable" ~ "Not given"))


# Compute key statistics
prospective_reg_iv <- round(mean(terminated_intovalue$is_prospective) * 100)
summary_results_iv <- round(mean(terminated_intovalue$has_summary_result) * 100)
publication_iv <- round(mean(terminated_intovalue$has_publication) * 100)

#  Any result reporting (SR or publication)
terminated_intovalue <- 
  terminated_intovalue |>
  mutate(days_to_publ = pmin(days_cd_to_publication,   #get minimum of days to pub or to summary result
                             days_cd_to_summary, na.rm = TRUE)) |>
  mutate(has_publ_or_summary = (has_publication)=="TRUE" | (has_summary_result=="TRUE"))

any_result_iv <- round(mean(terminated_intovalue$has_publ_or_summary) * 100)

# Generate table for Intovalue terminated trials data
terminated_intovalue_table <- furniture::table1(
  terminated_intovalue, 
  phase_recoded_iv, 
  summary_results_iv, 
  publication_iv, 
  any_result_iv, 
  na.rm = FALSE
)

colnames(terminated_intovalue)


# --- Part 2: Contrast Data Analysis ----
# Read and filter data for Contrast

# Read data for Contrast terminated trials
terminated_contrast <- terminated_cthist |>
  filter(source == "contrast")

# Apply functions to generate characteristics for terminated trials
functions_list <- list(
  get_why_stopped = get_why_stopped,
  get_anticipated_enrollment = get_anticipated_enrollment,
  get_actual_enrollment = get_actual_enrollment,
  has_summary_result = has_summary_result,
  get_start_date = get_start_date,
  get_stop_date = get_stop_date
)

terminated_contrast <- 
  functions_list |>
  map_dfc(~ .x(terminated_contrast$nctid, historical_version = TRUE, terminated_contrast)) |>
  select(nctid...1, reason_for_termination, anticipated_enrollment,actual_enrollment, has_summary_result, start_date, stop_date ) |>
  rename(nctid = nctid...1)

# Calculate degree of enrollment and trial duration
terminated_contrast <- duration_of_trial(terminated_contrast, "start_date", "stop_date")

trial_days_stats <- 
  terminated_contrast |>
  reframe(
    mean = mean(trial_days, na.rm = TRUE),  
    range = range(trial_days, na.rm = TRUE)
    
  )

enrollment_stats <- degree_of_enrollment(terminated_intovalue, anticipated_column =  "anticipated_enrollment", actual_column =  "actual_enrollment")


# Get other characteristics from original Contrast data
terminated_contrast <- 
  terminated_contrast |>
  select(-start_date) |>
  left_join(
    contrast_raw, 
    by = c("nctid" = "id")
  ) |>
  distinct(nctid, .keep_all = TRUE)

# Recode variables
terminated_contrast <- 
  mutate(terminated_contrast, phase_recoded_ct = case_when(
    phase == "Early Phase 1" | phase == "1" | phase == "Phase 1" ~ "I",
    phase == "Phase 1/Phase 2" ~ "I-II",
    phase == "2" | phase == "Phase 2" ~ "II",
    phase == "Phase 2/Phase 3" ~ "II-III",
    phase == "3" | phase == "Phase 3" ~ "III",
    phase == "4" | phase == "Phase 4" ~ "IV",
    phase == "0" | phase == "Not Applicable" ~ "Not given"))


# Compute key statistics
prospective_reg_ct <- round(mean(terminated_contrast$is_prospective) * 100)
summary_results_ct <- round(mean(terminated_contrast$has_summary_results) * 100)
publication_ct <- round(mean(terminated_contrast$has_publication) * 100)


#days_cd_to_publication
terminated_contrast <- mutate(terminated_contrast, days_cd_to_publication = 
                                as.integer(ymd_hms(publication_date)-ymd_hms(completion_date)))
summary(terminated_contrast$days_cd_to_publication)

#days_cd_to_summary
terminated_contrast <- mutate(terminated_contrast, days_cd_to_summary = 
                                as.integer(ymd_hms(summary_results_date)-ymd_hms(completion_date)))
summary(terminated_contrast$days_cd_to_summary)

#  Any result reporting (SR or publication)
terminated_contrast <- 
  terminated_contrast |>
  mutate(days_to_publ = pmin(days_cd_to_publication,   #get minimum of days to pub or to summary result
                             days_cd_to_summary, na.rm = TRUE)) |>
  mutate(has_publ_or_summary = (has_publication)== "TRUE" | (has_summary_results== "TRUE"))

any_result_ct <- mean(terminated_contrast$has_publ_or_summary) * 100

# Generate table for Intovalue terminated trials data
terminated_contrast_table <- furniture::table1(
  terminated_contrast, 
  phase_recoded_ct, 
  summary_results_ct, 
  publication_ct, 
  any_result_ct, 
  na.rm = FALSE
)

colnames(terminated_contrast)

# Combine datasets with source column added
terminated_combined <- bind_rows(
  mutate(terminated_intovalue, source = "Intovalue") |> 
    select(nctid, reason_for_termination, anticipated_enrollment, actual_enrollment, has_summary_result, start_date, stop_date, trial_days, source),
  
  mutate(terminated_contrast, source = "Contrast") |> 
    select(nctid, reason_for_termination, anticipated_enrollment, actual_enrollment, has_summary_result, start_date, stop_date, trial_days, source)
)

#write.csv(terminated_combined, here::here("data", "processed", "terminated_combined_1.csv"), row.names = FALSE)
