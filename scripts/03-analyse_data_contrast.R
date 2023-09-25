# This script analysis terminated trials from CONTRAST dataset and is based on '01_analyse_data_terminated.R' 
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

# reading CONTRAST raw data [TODO: download historical version for raw CONTRAST data]
contrast_raw <- read.csv(here::here("data","processed","contrast", "California-trials_2014-2017_exp.csv"), sep = ";")# use cthist to get historical data entry for contrast terminated trials (download date: 2023-08-04)

#contrast_cthist <- cthist::clinicaltrials_gov_download(contrast_raw_terminated$id)
contrast_cthist_raw <- read.csv(here::here("data","processed","contrast", "cthist_contrast_1.csv")) 

# apply terminated exclusion criteria (final status as 'terminated' and enrolment > 1)-----
contrast_terminated_cthist <- 
  contrast_cthist_raw |>
  group_by(nctid) |>
  mutate(
    final_version = last(total_versions),  
    final_status = last(overall_status),   
    final_enrollment = ifelse(version_number == final_version, enrolment, NA)  # Enrollment for the final version
  ) |>
  filter(final_status == "TERMINATED" & final_enrollment > 1 | is.na(final_enrollment)) |>
  ungroup()


# create reason for termination variable (refers to the reason of termination for a clinical trial)----
contrast_terminated_cthist <- 
  contrast_terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(why_stopped = ifelse(any(nzchar(whystopped)), dplyr::last(na.omit(whystopped)),NA_character_)) |> 
  dplyr::ungroup() 

# create has_summary_result_ctgov variable (refers to summary resulted posted in ClinicalTrial.gov)----
contrast_terminated_cthist <- 
  contrast_terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(has_summary_result_ctgov = ifelse(dplyr::last(results_posted) == "TRUE", TRUE, FALSE)) |> 
  dplyr::ungroup()


## time of termination (start_date, stop date) ----

# create a 'stop_date' variable (when trial overall status was first updated to terminated in registry)
contrast_terminated_cthist <- 
  contrast_terminated_cthist |>
  group_by(nctid) |>
  mutate(stop_date = dplyr::if_else(overall_status == "TERMINATED", as.character(version_date), NA_character_),
                stop_date = min(stop_date, na.rm = TRUE)) |>
  ungroup()

# create a 'start_date' (when trial was started and taken from same version when  trial overall status was first updated to terminated in registry)
contrast_terminated_cthist <- 
  contrast_terminated_cthist |>
  group_by(nctid) |>
  mutate(
  start_date = if_else(!is.na(stop_date), 
  as.character(study_start_date[which.max(version_date == stop_date)]), NA_character_)
  ) |>
  ungroup()


## degree of recruitment (anticipated and actual enrollment) ----

# create a 'anticipated enrollment' variable (refers to number of participants at trial launch)
contrast_terminated_cthist <- 
  contrast_terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(anticipated_enrollment = ifelse(enrolment_type == "ESTIMATED", as.integer(enrolment), NA_integer_),
                anticipated_enrollment = last(na.omit(anticipated_enrollment))) |> 
  dplyr::ungroup()

# create a 'actual enrollment' variable (refers to the observed number of participants who are actually enrolled)
contrast_terminated_cthist <- 
  contrast_terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(actual_enrollment = ifelse(enrolment_type == "ACTUAL", as.integer(enrolment), NA_integer_),
                actual_enrollment = dplyr::last(actual_enrollment)) |> 
  dplyr::ungroup()


# keep unique observations generated for each trial ----
contrast_terminated_cthist_updated <-
  contrast_terminated_cthist |> 
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
contrast_terminated_cthist_updated <- 
  terminatedtrialsstudy::degree_of_enrollment(contrast_terminated_cthist_updated,"anticipated_enrollment","actual_enrollment", round_off = 2)
  
# generate duration of enrollment by using function 'duration_of_trial' ----
contrast_terminated_cthist_updated <- 
  duration_of_trial(contrast_terminated_cthist_updated, "start_date", "stop_date")

