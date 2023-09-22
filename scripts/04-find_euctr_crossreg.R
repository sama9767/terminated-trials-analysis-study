# This script's finds and generate EUCTR cross-registrations for terminated ClinicalTrials.gov(ctgov) 
# trials within the IntoValue dataset. It achieves this via two methods:

# 1. Finding additional registry identifiers in ClinicalTrials.gov: 
# Get registry identifiers within the ctgov database (source: https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trn/cross-registrations.rds)

# 2. Finding additional registry identifiers in EUCTR registry:
# Get EUCTR ids hosted by German UMC included in IntoValue dataset(source: https://github.com/ebmdatalab/euctr-tracker-data)
# Get additional registry identifiers for this EUCTR ids by using forked euctrscrape github repository (source: https://github.com/delwen/euctrscrape)
# Extract ctgov trials from this additional registry identifiers
# Check for overlap between Intovalue ctgov trials and the extracted ctgov trials from additional registry identifiers

library(jsonlite)
library(httr)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(vroom)
library(here)
library(readr)

## get intovalue ctgov terminated trials

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
    !(is_dupe & iv_version == 1),
    # trials registered at Clinicaltrials.gov
    registry == "ClinicalTrials.gov",
    # trials that are terminated
    recruitment_status == "Terminated"
  ) |>
  # select required columns 
  dplyr::select(
    id, has_summary_results, 
  )



# Finding additional registry identifiers in ClinicalTrials.gov ---------------------------

# get cross-registered data (source:)
crossreg_raw <- readr::read_rds("https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trn/cross-registrations.rds?raw=true")

# filter clinicalTrials.gov Ids with cross-registration in EUCTR registry
euctr_in_ctgov <- 
  crossreg_raw |>
  filter(crossreg_registry == "EudraCT") |>
  rename(euctr_id = crossreg_trn) |>
  filter(grepl("^nct", id, ignore.case = TRUE)) |>
  select(id, euctr_id) |>
  mutate(crossreg_euctr_in_ctgov = TRUE)


# Finding additional registry identifiers in EUCTR registry ----------------------------------------
# Note: This part of code is written by Vladislav Nachev and Delwen Franzen (https://github.com/quest-bih/ctt-discussions/blob/main/scripts/02_get-german-euctr-ids.R)

## Step 1: Get EUCTR ids hosted by German UMC included in IntoValue dataset


# get data (source: https://github.com/ebmdatalab/euctr-tracker-data)
all_trials <- read_json("https://raw.githubusercontent.com/ebmdatalab/euctr-tracker-data/master/all_trials.json")

# get lookup table with German UMC and relevant name(s) in EUTT
DE_UMCs <- vroom::vroom(here("data","raw","intovalue", "GermanUMCs.csv")) |>
  mutate(sponsor = str_split(sponsor, "; ")) |>
  unnest(sponsor)

# create tibble and extract information from all_trials
trials_tidy <- tibble(id = map_chr(all_trials, pluck("trial_id")),
                      sponsor = map_chr(all_trials, pluck("normalized_name")),
                      title = map_chr(all_trials, pluck("trial_title")))


# get title and id for EUCTR registered trial with German sponsor
euctr_trials_german_umc <- trials_tidy |>
  filter(sponsor %in% DE_UMCs$sponsor) |>
  left_join(DE_UMCs, by = "sponsor") |>
  mutate(title_len = str_length(title)) 

## Step 2 Get additional registry identifiers for this EUCTR ids

# get function to extract additional identifiers for EUCTR IDs with German UMC of IV  dataset
source("https://raw.githubusercontent.com/delwen/euctrscrape/main/R/euctr_download.R")
source("https://raw.githubusercontent.com/delwen/euctrscrape/main/R/euctr_extract.R")

# extract additional identifiers for EUCTR ids with German UMC lead (this operation takes ~4 hours)
#euctr-crossreg-id-raw-dataset <- combine_info(trials_tidy$id)

# get dataset for additional identifiers of EUCTR IDs with German UMC 
euctr_crossreg_id_raw <- read.csv(here::here("data","raw","intovalue", "euctr-crossreg-id-raw-dataset.csv"))


## Step 3 Extract ctgov trials from this additional registry identifiers

# filter EUCTR ids with ClinicalTrials.gov IDs
euctr_crossreg_id <- 
  euctr_crossreg_id_raw |> group_by(euctr_id) |>
  filter(!is.na(other_id) & field == "US NCT number")  |>
  rename(crossreg_ctgov_in_euctr = other_id) |>
  select(crossreg_ctgov_in_euctr, euctr_id)


## Check for overlap between Intovalue ctgov trials and the extracted ctgov trials from additional registry identifiers

# check overlap 
ctgov_in_euctr <- iv_terminated |>
  inner_join(euctr_crossreg_id, by = c("id" = "crossreg_ctgov_in_euctr")) |>
  mutate(crossreg_ctgov_in_euctr = TRUE) |>
  select(id, crossreg_ctgov_in_euctr,euctr_id)


# Combine the bidirectional cross-registration ids------------------------------
crossreg_ids <- merge(euctr_in_ctgov,ctgov_in_euctr, by = "id", all = TRUE) |>
  mutate(crossreg_euctr_id = coalesce(euctr_id.x, euctr_id.y)) |>
  select(id,crossreg_euctr_id)


# find matching cross-registration ids in intovalue terminated dataset
matching_rows <- match(iv_terminated$id, crossreg_ids$id)

# join information in intovalue terminated dataset
iv_terminated$crossreg_euctr_id <- ifelse(!is.na(matching_rows), crossreg_ids$crossreg_euctr_id[matching_rows], NA)

# add boolean to indicate cross-registration
iv_terminated_crossreg <- iv_terminated |>
  mutate(is_crossreg_eudract = if_else(!is.na(crossreg_euctr_id), TRUE, FALSE))


#write.csv(iv_terminated_crossreg,file = here::here("data", "processed", "intovalue", "iv_terminated_crossreg.csv"),row.names = FALSE)