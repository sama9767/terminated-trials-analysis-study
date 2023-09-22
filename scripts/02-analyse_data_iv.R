#' This script is based on '01_analyse_data_terminated.R' and generates following variables 
#' for terminated trials in  IntoValue dataset (link:https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trials.rds)
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

#remotes::install_github("sama9767/terminated-trials-study")
#library(terminatedtrialsstudy)
library(dplyr)

# load duration_of_enrollment function
source(here::here("scripts", "functions", "duration_of_trial.R"))

# load degree_of_enrollment function
source(here::here("scripts", "functions", "degree_of_enrollment.R"))


# reading Intovalue raw data
intovalue_raw <- rio::import("https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trials.rds?raw=true")

# prepare Intovalue data with terminated trial study inclusion criteria  
iv_terminated <- intovalue_raw %>%
  dplyr::filter(
    iv_completion,
    iv_status,
    iv_interventional,
    has_german_umc_lead,
    # In case of dupes, exclude IV1 version
    !(is_dupe & iv_version == 1),
    # trials registered at Clinicaltrials.gov
    registry == "ClinicalTrials.gov",
    # trials that are terminated
    recruitment_status == "Terminated"
  ) 

# use cthist to get historical data entry for iv terminated trials (download date: 2023-08-02)
#iv_terminated_cthist <- cthist::clinicaltrials_gov_download(iv_terminated$id)
# note: information for two nct id couldnt be downloaded (NCT00150878,NCT01837082)
iv_terminated_cthist <- read.csv(here::here("data","processed","intovalue", "cthist_iv_1.csv"))


# create reason for termination variable (refers to the reason of termination for a clinicial trial)------
iv_terminated_cthist <- iv_terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(why_stopped = ifelse(any(nzchar(whystopped)), dplyr::last(na.omit(whystopped)),NA_character_)) %>% 
  dplyr::ungroup() 

# crate has_summary_result_ctgov variable (refers to summary resulted posted in ClinicalTrial.gov)
iv_terminated_cthist <- iv_terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(has_summary_result_ctgov = ifelse(dplyr::last(results_posted) == "TRUE", TRUE, FALSE)) %>% 
  dplyr::ungroup()


## time of termination (start_date, stop date)----------------------------------

# create a 'start_date' variable (when trial was started)
iv_terminated_cthist <- iv_terminated_cthist %>%  dplyr::group_by(nctid) %>%
  dplyr::mutate(start_date =  dplyr::last(study_start_date)) %>% dplyr::ungroup()

# create a 'stop_date' variable (when trial overall status was first updated to terminated in registry)
iv_terminated_cthist <- iv_terminated_cthist %>%
  dplyr::group_by(nctid) %>%
  dplyr::mutate(stop_date = dplyr::if_else(overall_status == "TERMINATED", as.character(version_date), NA_character_),
                stop_date = min(stop_date, na.rm = TRUE)) %>%
  dplyr::ungroup()



## degree of recruitment (anticipated and actual enrollment, percentage of enrollment)-------------------

# create a 'anticipated enrollment' variable (refers to the expected number of participants that the trial aims to enroll)
iv_terminated_cthist <- iv_terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(anticipated_enrollment = ifelse(enrolment_type == "ESTIMATED", as.integer(enrolment), NA_integer_),
                anticipated_enrollment = last(na.omit(anticipated_enrollment))) %>% 
  dplyr::ungroup()

# create a 'actual enrollment' variable (refers to the observed number of participants who are actually enrolled)
iv_terminated_cthist <- iv_terminated_cthist %>% 
  dplyr::group_by(nctid) %>% 
  dplyr::mutate(actual_enrollment = ifelse(enrolment_type == "ACTUAL", as.integer(enrolment), NA_integer_),
                actual_enrollment = last(actual_enrollment)) %>% 
  dplyr::ungroup()


# keep unique observations generated for each trial
iv_terminated_cthist_updated <- iv_terminated_cthist %>% 
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
iv_terminated_cthist_updated <-  degree_of_enrollment(iv_terminated_cthist_updated,anticipated_column = "anticipated_enrollment",actual_column = "actual_enrollment")

# generate duration of enrollment by using function 'duration_of_trial'
iv_terminated_cthist_updated <- duration_of_trial(iv_terminated_cthist_updated, "start_date", "stop_date")

# save dataset
#write.csv(iv_terminated_cthist_updated,file = here::here("data", "processed", "intovalue", "iv_terminated_cthist_updated.csv"),row.names = FALSE)