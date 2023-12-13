#remotes::install_github("sama9767/terminated-trials-study")
library(terminatedtrialsstudy)
library(dplyr)
library(readr)


# read raw data
cthist_raw <- read_csv(here::here("data", "processed", "2023-12-01-historical-versions.csv"))

# apply terminated exclusion criteria (final status as 'terminated' and enrolment > 1)-----
cthist_terminated <- cthist_raw |>
  group_by(nctid) |>
  summarise(
    final_status = last(overall_status),
    total_enrollment = sum(enrolment)
  ) |>
  filter(final_status == "TERMINATED" & total_enrollment > 1) |>
  ungroup()


# Filter the original dataset based on selected nctid
cthist_terminated <- 
  cthist_raw |>
  filter(nctid %in% cthist_terminated$nctid)
  

# get all variables
terminated_whystopped <- terminatedtrialsstudy::get_why_stopped(cthist_terminated$nctid, historical_version = TRUE, cthist_terminated)


# get other variable (testing phase)
cthist_terminated_2 <- terminatedtrialsstudy::get_anticipated_enrollment(cthist_terminated$nctid, historical_version = TRUE, cthist_terminated)
cthist_terminated_3 <- terminatedtrialsstudy::get_actual_enrollment(cthist_terminated$nctid, historical_version = TRUE, cthist_terminated)
cthist_terminated_5 <- terminatedtrialsstudy::has_summary_result(cthist_terminated$nctid, historical_version = TRUE, cthist_terminated)
cthist_terminated_6 <- terminatedtrialsstudy::get_start_date(cthist_terminated$nctid, historical_version = TRUE, cthist_terminated)
cthist_terminated_7 <- terminatedtrialsstudy::get_stop_date (cthist_terminated$nctid, historical_version = TRUE, cthist_terminated)

