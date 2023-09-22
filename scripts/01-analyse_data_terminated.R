# Script Documentation: Processing Terminated ClinicalTrials.gov Trials

# Description:
# This script processes the ClinicalTrials.gov registered trials with a "Terminated" status. 
# It utilizes historical entries for these trials obtained using the 'cthist' package by BG Carlisle. 


# Input:
# The dataset should contain relevant information about the trials, including trial ID (nctid) and its recruitment status ('recruitment_status')
# The input data should be the ClinicalTrials.gov registered trials with a "Terminated" status in registry. 

# Functions
# The script uses duration_of_trial and degree_of_enrollment function
# duration_of_trial generates number of days trial was ongoing until termination (i.e trial_days)
# degree_of_enrollment generates percentage of enrollment for particular terminated trial (i.e enrollment_percentage)

# Output:
# The output dataset includes the following variables for each terminated trial:
# - nctid: Trial ID
# - start_date: Date when the trial was started
# - stop_date: Date when the trial's overall status was first updated to "Terminated" in the registry
# - why_stopped: The reason for trial termination
# - has_summary_result_ctgov: Boolean indicating whether summary results are available on ClinicalTrials.gov for the trial
# - anticipated_enrollment: The expected number of participants that the trial aims to enroll
# - actual_enrollment: The observed number of participants who are actually enrolled
# - trial_days : The number of days was ongoing until termination
# - enrollment_percentage : The percentage of enrollment achieved until termination

library(dplyr)

# get NCT IDs with status 'terminated' (assumes NCT IDs column name as 'nctid')
raw_data <- here(path/to/file/raw_data.csv)

# prepare the data for terminated trials 
raw_trials <- raw_data


# prepare raw data (optional step to filter raw data for trials registered in ClincialTrials.gov with "Terminated status" )
terminated_trials <- raw_trials %>%
  dplyr::filter(registry == "ClinicalTrials.gov",
                # trials that are terminated
                recruitment_status == "Terminated") 

# retrieve historical data using cthist for terminated trials------
terminated_cthist <- cthist::clinicaltrials_gov_download(terminated_trials$id)


# create reason for termination variable (refers to the reason of termination for a clinical trial)
terminated_cthist <- terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(why_stopped = ifelse(any(nzchar(whystopped)), dplyr::last(na.omit(whystopped)),NA_character_)) %>% 
  dplyr::ungroup() 

# crate has_summary_result_ctgov variable (refers to summary resulted posted in ClinicalTrial.gov)
terminated_cthist <- terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(has_summary_result_ctgov = ifelse(dplyr::last(results_posted) == "TRUE", TRUE, FALSE)) %>% 
  dplyr::ungroup()


## time of termination (start_date, stop date)

# create a 'start_date' variable (when trial was started)
terminated_cthist <- terminated_cthist %>%  dplyr::group_by(nctid) %>%
  dplyr::mutate(start_date = dplyr::last(study_start_date)) %>% dplyr::ungroup()

# create a 'stop_date' variable (when trial overall status was first updated to terminated in registry)
terminated_cthist <- terminated_cthist %>%
  dplyr::group_by(nctid) %>%
  dplyr::mutate(stop_date = dplyr::if_else(overall_status == "TERMINATED", as.character(version_date), NA_character_),
                stop_date = min(stop_date, na.rm = TRUE)) %>%
  dplyr::ungroup()



## degree of recruitment (anticipated and actual enrollment, percentage of enrollment)

# create a 'anticipated enrollment' variable (refers to the expected number of participants that the trial aims to enroll)
terminated_cthist <- terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(anticipated_enrollment = ifelse(enrolment_type == "ESTIMATED", as.integer(enrolment), NA_integer_),
                anticipated_enrollment = last(na.omit(anticipated_enrollment))) %>% 
  dplyr::ungroup()

# create a 'actual enrollment' variable (refers to the observed number of participants who are actually enrolled)
terminated_cthist <- terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(actual_enrollment = ifelse(enrolment_type == "ACTUAL", as.integer(enrolment), NA_integer_),
                actual_enrollment = last(actual_enrollment)) %>% 
  dplyr::ungroup()



# keep unique observations generated for each trial
terminated_cthist_updated <- terminated_cthist %>% 
  dplyr::group_by(nctid) %>%
  dplyr::mutate(
    anticipated_enrollment = anticipated_enrollment,
    actual_enrollment = actual_enrollment,
    start_date = na.omit(start_date)[1]
  ) %>% 
  dplyr::ungroup() %>%
  # select required columns
  dplyr::select(nctid, why_stopped, has_summary_result_ctgov, anticipated_enrollment, actual_enrollment, start_date, stop_date) %>%
  # keep distinct observation
  dplyr::distinct(nctid, .keep_all = TRUE) 

# generate enrollment_percentage by using function 'degree_of_enrollment'
iv_terminated_cthist_updated <- degree_of_enrollment(iv_terminated_cthist_updated,anticipated_column = "anticipated_enrolLment",
                                                     actual_column = "actual_enrolLment")

# generate duration of enrollment by using function 'duration_of_trial'
iv_terminated_cthist_updated <- duration_of_trial(iv_terminated_cthist_updated, "start_date", "stop_date")