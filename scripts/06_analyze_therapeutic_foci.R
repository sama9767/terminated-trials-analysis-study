# Analyze therapeutic focus

# Load necessary libraries
library(dplyr)
library(furniture)
library(readr)
library(here)
library(TrialFociMapper)

# ---------------------------------------------------------------
# Example: How to fetch therapeutic foci using TrialFociMapper package
# (This is provided as an example and is commented out)
# For terminated Intovalue trials:
# terminated_foci_intovalue_raw <- TrialFociMapper::get_foci_ctgov(terminated_intovalue$nctid, 
#                                  username = "username", password = "password")
# terminated_foci_intovalue_processed <- TrialFociMapper::assign_therapeutic_focus(
#                                  data = terminated_foci_intovalue_raw, 
#                                  nctid_col = "nct_id", 
#                                  mesh_heading_cols = "trial_foci_table_list")

# ---------------------------------------------------------------

# Load preprocessed therapeutic foci datasets
completed_foci_intovalue <- readRDS(here::here("data", "processed", "therapeutic_foci" ,"completed_foci_intovalue.rds"))
completed_foci_contrast <- readRDS(here::here("data", "processed", "therapeutic_foci","completed_foci_contrast.rds"))
terminated_foci_intovalue <- readRDS(here::here("data", "processed", "therapeutic_foci", "terminated_foci_intovalue.rds"))
terminated_foci_contrast <- readRDS(here::here("data", "processed", "therapeutic_foci", "terminated_foci_contrast.rds"))


# Function to clean data
clean_foci_data <- function(df) {
  df |>
    mutate(trial_foci_table_list = ifelse(lengths(trial_foci_table_list) == 0, "No Foci Entry", trial_foci_table_list)) |>
    unnest(trial_foci_table_list) # Unnest to separate foci
}

# Clean each dataset
completed_foci_intovalue <- clean_foci_data(completed_foci_intovalue)
completed_foci_contrast <- clean_foci_data(completed_foci_contrast)
terminated_foci_intovalue <- clean_foci_data(terminated_foci_intovalue)
terminated_foci_contrast <- clean_foci_data(terminated_foci_contrast)

# Combine datasets and add labels
all_foci_data <- bind_rows(
  completed_foci_intovalue %>% mutate(source = "Completed Intovalue"),
  completed_foci_contrast %>% mutate(source = "Completed Contrast"),
  terminated_foci_intovalue %>% mutate(source = "Terminated Intovalue"),
  terminated_foci_contrast %>% mutate(source = "Terminated Contrast")
)

# Generate summary table
all_foci_table <- furniture::table1(
  all_foci_data,
  trial_foci_table_list, splitby = "source",
  na.rm = TRUE
)