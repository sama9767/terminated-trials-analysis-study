#' This script is based on '01_analyse_data_terminated.R' and generates following variables 
#' for terminated trials in Contrast dataset (link:https://github.com/maia-sh/california-trials)
#
# - nctid: Trial ID
# - start_date: Date when the trial was started
# - stop_date: Date when the trial's overall status was first updated to "Terminated" in the registry
# - why_stopped: The reason for trial termination
# - has_summary_result_ctgov: Boolean indicating whether summary results are available on ClinicalTrials.gov for the trial
# - anticipated_enrollment: The expected number of participants that the trial aims to enroll
# - actual_enrollment: The observed number of participants who are actually enrolled
# - trial_days : The number of days trial was ongoing until termination
# - enrollment_percentage : The percentage of enrollment achieved until termination

library(dplyr)

# load duration_of_enrollment function
source(here::here("scripts", "functions", "duration_of_trial.R"))

# load degree_of_enrollment function
source(here::here("scripts", "functions", "degree_of_enrollment.R"))


# reading CONTRAST raw data
contrast_raw <- read.csv(here::here("data","processed","contrast", "California-trials_2014-2017_exp.csv"), sep = ";")

# prepare CONTRAST data 
contrast_raw_terminated <- contrast_raw %>%
  ## trials that are terminated
  dplyr::filter(recruitment_status == "Terminated") 


# use cthist to get historical data entry for contrast terminated trials (download date: 2023-08-04)
#contrast_cthist <- cthist::clinicaltrials_gov_download(contrast_raw_terminated$id)
contrast_terminated_cthist <- read.csv(here::here("data","processed","contrast", "cthist_contrast_1.csv")) 


# create reason for termination variable (refers to the reason of termination for a clinical trial)
contrast_terminated_cthist <- contrast_terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(why_stopped = ifelse(any(nzchar(whystopped)), dplyr::last(na.omit(whystopped)),NA_character_)) %>% 
  dplyr::ungroup() 

# crate has_summary_result_ctgov variable (refers to summary resulted posted in ClinicalTrial.gov)
contrast_terminated_cthist <- contrast_terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(has_summary_result_ctgov = ifelse(dplyr::last(results_posted) == "TRUE", TRUE, FALSE)) %>% 
  dplyr::ungroup()


## time of termination (start_date, stop date)

# create a 'start_date' variable (when trial was started)
contrast_terminated_cthist <- contrast_terminated_cthist %>%  dplyr::group_by(nctid) %>%
  dplyr::mutate(start_date = dplyr::last(study_start_date)) %>% dplyr::ungroup()

# create a 'stop_date' variable (when trial overall status was first updated to terminated in registry)
contrast_terminated_cthist <- contrast_terminated_cthist %>%
  dplyr::group_by(nctid) %>%
  dplyr::mutate(stop_date = dplyr::if_else(overall_status == "TERMINATED", as.character(version_date), NA_character_),
                stop_date = min(stop_date, na.rm = TRUE)) %>%
  dplyr::ungroup()



## degree of recruitment (anticipated and actual enrollment, percentage of enrollment)

# create a 'anticipated enrollment' variable (refers to the expected number of participants that the trial aims to enroll)
contrast_terminated_cthist <- contrast_terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(anticipated_enrollment = ifelse(enrolment_type == "ESTIMATED", as.integer(enrolment), NA_integer_),
                anticipated_enrollment = last(na.omit(anticipated_enrollment))) %>% 
  dplyr::ungroup()

# create a 'actual enrollment' variable (refers to the observed number of participants who are actually enrolled)
contrast_terminated_cthist <- contrast_terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(actual_enrollment = ifelse(enrolment_type == "ACTUAL", as.integer(enrolment), NA_integer_),
                actual_enrollment = dplyr::last(actual_enrollment)) %>% 
  dplyr::ungroup()


# keep unique observations generated for each trial
contrast_terminated_cthist_updated <- contrast_terminated_cthist %>% 
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
contrast_terminated_cthist_updated <- degree_of_enrollment(contrast_terminated_cthist_updated,anticipated_column = "anticipated_enrollment",actual_column = "actual_enrollment")

# generate duration of enrollment by using function 'duration_of_trial'
contrast_terminated_cthist_updated <- duration_of_trial(contrast_terminated_cthist_updated, "start_date", "stop_date")