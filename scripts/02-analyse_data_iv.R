# This script analysis terminated trials from IntoValue dataset and is based on '01_analyse_data_terminated.R' 
#
# Following variables are generated:
# - nctid: Trial ID
# - stop_date: Date when the trial's overall status was first updated to "Terminated" in the registry
# - start_date: Date when the trial was started (taken from the same version of the trial record when the trial was updated as ‘Terminated’)
# - why_stopped: The reason for trial termination
# - has_summary_result_ctgov: Boolean indicating whether summary results are available on ClinicalTrials.gov 
# - anticipated_enrollment: The expected number of participants that the trial aims to enroll
# - actual_enrollment: The observed number of participants who are actually enrolled
# - trial_days : The number of days trial was ongoing until termination
# - enrollment_percentage : The percentage of enrollment achieved until termination


#remotes::install_github("sama9767/terminated-trials-study")
library(terminatedtrialsstudy)
library(dplyr)

# load duration_of_enrollment function
source(here::here("scripts", "functions", "duration_of_trial.R"))

# reading Intovalue raw data
intovalue_raw <- rio::import("https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trials.rds?raw=true")

# prepare Intovalue data with terminated trial study inclusion criteria  
iv_terminated <- intovalue_raw |>
  dplyr::filter(
    iv_completion,
    iv_status,
    iv_interventional,
    has_german_umc_lead,
    # In case of dupes, exclude IV1 version
    !(is_dupe & iv_version == 1))
    
# use cthist to get historical data entry for iv terminated trials (download date: 2023-08-02)
#iv_terminated_cthist <- cthist::clinicaltrials_gov_download(iv_terminated$id)
# note: information for two nct id couldn't be downloaded (NCT00150878,NCT01837082)
iv_terminated_cthist_raw <- read.csv(here::here("data","processed","intovalue", "cthist_iv_1.csv"))

# apply terminated exclusion criteria (final status as 'terminated' and enrolment > 1)-----
iv_terminated_cthist <- 
  iv_terminated_cthist_raw |>
  group_by(nctid) |>
  mutate(
    final_version = last(total_versions),  
    final_status = last(overall_status),   
    final_enrollment = ifelse(version_number == final_version, enrolment, NA)  
  ) |>
  filter(final_status == "TERMINATED" & final_enrollment > 1 | is.na(final_enrollment)) |>
  ungroup()

# create reason for termination variable (refers to the reason of termination for a clinicial trial)------
iv_terminated_cthist <- iv_terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(why_stopped = ifelse(any(nzchar(whystopped)), dplyr::last(na.omit(whystopped)),NA_character_)) |> 
  dplyr::ungroup() 

# create has_summary_result_ctgov variable (refers to summary resulted posted in ClinicalTrial.gov)
iv_terminated_cthist <- iv_terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(has_summary_result_ctgov = ifelse(dplyr::last(results_posted) == "TRUE", TRUE, FALSE)) |> 
  dplyr::ungroup()


## time of termination (start_date, stop date) --------

# create a 'stop_date' variable (when trial overall status was first updated to terminated in registry)
iv_terminated_cthist <- 
  iv_terminated_cthist |>
  group_by(nctid) |>
  mutate(stop_date = dplyr::if_else(overall_status == "TERMINATED", as.character(version_date), NA_character_),
         stop_date = min(stop_date, na.rm = TRUE)) |>
  ungroup()

# create a 'start_date' (when trial was started and taken from same version when  trial overall status was first updated to terminated in registry)
iv_terminated_cthist <- 
  iv_terminated_cthist |>
  group_by(nctid) |>
  mutate(
    start_date = if_else(!is.na(stop_date), 
                         as.character(study_start_date[which.max(version_date == stop_date)]), NA_character_)
  ) |>
  ungroup()


## degree of recruitment (anticipated and actual enrollment) ------

# create a 'anticipated enrollment' variable (refers to the expected number of participants that the trial aims to enroll)
iv_terminated_cthist <- 
  iv_terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(anticipated_enrollment = ifelse(enrolment_type == "ESTIMATED", as.integer(enrolment), NA_integer_),
                anticipated_enrollment = last(na.omit(anticipated_enrollment))) |> 
  dplyr::ungroup()

# create a 'actual enrollment' variable (refers to the observed number of participants who are actually enrolled)
iv_terminated_cthist <- 
  iv_terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(actual_enrollment = ifelse(enrolment_type == "ACTUAL", as.integer(enrolment), NA_integer_),
                actual_enrollment = dplyr::last(actual_enrollment)) |> 
  dplyr::ungroup()


# keep unique observations generated for each trial -----
iv_terminated_cthist_updated <- iv_terminated_cthist |> 
  dplyr::group_by(nctid) |>
  dplyr::mutate(
    anticipated_enrollment = anticipated_enrollment,
    actual_enrollment = actual_enrollment,
    start_date = na.omit(start_date)[1]
  ) |> 
  dplyr::ungroup() |>
  # select required columns
  dplyr::select(nctid, why_stopped, has_summary_result_ctgov, anticipated_enrollment, actual_enrollment, start_date, stop_date) |>
  # keep distinct observation
  dplyr::distinct(nctid, .keep_all = TRUE) 


# generate enrollment_percentage by using function 'degree_of_enrollment' ----
#NOTE: Error: Actual enrollment is not an integer
iv_terminated_cthist_updated <- terminatedtrialsstudy::degree_of_enrollment(iv_terminated_cthist_updated, anticipated_column = "anticipated_enrollment" , actual_column = "actual_enrollment", round_off = 2)

# generate duration of enrollment by using function 'duration_of_trial' -----
iv_terminated_cthist_updated <- duration_of_trial(iv_terminated_cthist_updated, "start_date", "stop_date")

# save dataset
#write.csv(iv_terminated_cthist_updated,file = here::here("data", "processed", "intovalue", "iv_terminated_cthist_updated.csv"),row.names = FALSE)
