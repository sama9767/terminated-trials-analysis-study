# Script Documentation: Processing Terminated ClinicalTrials.gov Trials

# Description:
# This script processes the ClinicalTrials.gov registered trials with a "Terminated" status. 
# It utilizes historical entries for these trials obtained using the 'cthist' package by BG Carlisle. 


# Input:
# The input data should be the ClinicalTrials.gov registered trials 

# Functions
# The script relies on function from 'terminated-trial-study' R package
# duration_of_trial generates number of days trial was ongoing until termination (i.e trial_days)
# degree_of_enrollment generates percentage of enrollment for particular terminated trial (i.e enrollment_percentage)

# Output:
# The output dataset includes the following variables for each terminated trial:
# - nctid: Trial ID
# - start_date: Date when the trial was started (taken from the same version of the trial record when the trial was updated as ‘Terminated’)
# - stop_date: Date when the trial's overall status was first updated to "Terminated" in the registry
# - why_stopped: The reason for trial termination
# - has_summary_result_ctgov: Boolean indicating whether summary results are available on ClinicalTrials.gov for the trial
# - anticipated_enrollment: The expected number of participants that the trial aims to enroll
# - actual_enrollment: The observed number of participants who are actually enrolled
# - trial_days : The number of days was ongoing until termination
# - enrollment_percentage : The percentage of enrollment achieved until termination

#remotes::install_github("sama9767/terminated-trials-study")
library(dplyr)
library(terminatedtrialsstudy)

# get NCT IDs with status 'terminated' (assumes NCT IDs column name as 'nctid')
raw_data <- here(path/to/file/raw_data.csv)

# prepare the data for terminated trials 
raw_trials <- raw_data


# retrieve historical data using cthist for terminated trials------
raw_trials_cthist <- cthist::clinicaltrials_gov_download(raw_trials$id)


# apply terminated exclusion criteria (final status as 'terminated' and enrolment > 1)-----
terminated_cthist <- 
  raw_trials_cthist |>
  group_by(nctid) |>
  mutate(
    final_version = last(total_versions),  
    final_status = last(overall_status),   
    final_enrollment = ifelse(version_number == final_version, enrolment, NA)  
  ) |>
  filter(final_status == "TERMINATED" & final_enrollment > 1 | is.na(final_enrollment)) |>
  ungroup()

# create reason for termination variable (refers to the reason of termination for a clinical trial)
terminated_cthist <- terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(why_stopped = ifelse(any(nzchar(whystopped)), dplyr::last(na.omit(whystopped)),NA_character_)) |> 
  dplyr::ungroup() 

# create has_summary_result_ctgov variable (refers to summary resulted posted in ClinicalTrial.gov)
terminated_cthist <- terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(has_summary_result_ctgov = ifelse(dplyr::last(results_posted) == "TRUE", TRUE, FALSE)) |> 
  dplyr::ungroup()


## time of termination (start_date, stop date) ---------

# create a 'anticipated enrollment' variable (refers to the expected number of participants that the trial aims to enroll)
terminated_cthist <- 
  terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(anticipated_enrollment = ifelse(enrolment_type == "ESTIMATED", as.integer(enrolment), NA_integer_),
                anticipated_enrollment = last(na.omit(anticipated_enrollment))) |> 
  dplyr::ungroup()

# create a 'actual enrollment' variable (refers to the observed number of participants who are actually enrolled)
terminated_cthist <- 
  terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(actual_enrollment = ifelse(enrolment_type == "ACTUAL", as.integer(enrolment), NA_integer_),
                actual_enrollment = dplyr::last(actual_enrollment)) |> 
  dplyr::ungroup()


## degree of recruitment (anticipated and actual enrollment) ---------

# create a 'anticipated enrollment' variable (refers to the expected number of participants that the trial aims to enroll)
terminated_cthist <- terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(anticipated_enrollment = ifelse(enrolment_type == "ESTIMATED", as.integer(enrolment), NA_integer_),
                anticipated_enrollment = last(na.omit(anticipated_enrollment))) |> 
  dplyr::ungroup()

# create a 'actual enrollment' variable (refers to the observed number of participants who are actually enrolled)
terminated_cthist <- terminated_cthist |> 
  dplyr::group_by(nctid) |> 
  dplyr::mutate(actual_enrollment = ifelse(enrolment_type == "ACTUAL", as.integer(enrolment), NA_integer_),
                actual_enrollment = last(actual_enrollment)) |> 
  dplyr::ungroup()



# keep unique observations generated for each trial
terminated_cthist_updated <- terminated_cthist |> 
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

# generate enrollment_percentage by using function 'degree_of_enrollment' -------
iv_terminated_cthist_updated <- terminatedtrialsstudy::degree_of_enrollment(iv_terminated_cthist_updated,anticipated_column = "anticipated_enrolLment",
                                                     actual_column = "actual_enrolLment")

# generate duration of enrollment by using function 'duration_of_trial' ----------
iv_terminated_cthist_updated <- terminatedtrialsstudy::duration_of_trial(iv_terminated_cthist_updated, "start_date", "stop_date")