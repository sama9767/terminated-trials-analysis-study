# Analyzing patient harm by comparing the risk of serious adverse events (SAEs) between intervention and control groups.

library(here)
library(readxl)
library(dplyr)
library(meta)
library(assertthat)

# Load patient harm data from ClinicalTrials.gov and EUCTR
patient_harm_ctgov_raw <- read_xlsx(here::here("data","manual", "manual-2024-04-29-sae_by_group.xlsx"), sheet = "Reconciliation_CTGOV")
patient_harm_euctr_raw <- read_xlsx(here::here("data","manual", "manual-2024-04-29-sae_by_group.xlsx"), sheet = "Reconciliation_EUCTR")

# Filter CTGOV trials included for patient harm analysis
patient_harm_ctgov <- 
  patient_harm_ctgov_raw |>
  group_by(nctid) |>
  filter(final_include_for_patient_harm == TRUE) |>
  select(nctid, group_id, seriousNumAffected, seriousNumAtRisk, final_arm_assigned, final_include_for_patient_harm)

# Count distinct ClinicalTrials.gov trials included
patient_harm_ctgov |> distinct(nctid) |> nrow()

# Filter EUCTR trials included for patient harm analysis
patient_harm_euctr <- patient_harm_euctr_raw |>
  group_by(id) |>
  filter(final_include_for_patient_harm == TRUE) |>
  select(id, group_id, seriousNumAffected, seriousNumAtRisk, final_arm_assigned, final_include_for_patient_harm) |>
  rename(nctid = id)

# Count distinct EUCTR trials included
patient_harm_euctr |> distinct(nctid) |> nrow()

# Combine ClinicalTrials.gov and EUCTR data for analysis
patient_harm <- rbind(patient_harm_ctgov, patient_harm_euctr)
patient_harm |> distinct(nctid) |> nrow()

# Aggregate similar trial arms for analysis
aggregated_data <- terminatedtrialsstudy::aggregate_arms(patient_harm, trial_id = "nctid", arm_assigned = "final_arm_assigned", affected_col = "seriousNumAffected", risk_col = "seriousNumAtRisk")

# Summarize data by trial for analysis
df_summary <- aggregated_data |>
  group_by(nctid) |>
  summarise(
    event_control = sum(total_seriousnumaffected[final_arm_assigned == "Control Arm"]),
    n_control = sum(total_seriousnumatrisk[final_arm_assigned == "Control Arm"]),
    event_intervention = sum(total_seriousnumaffected[final_arm_assigned == "Intervention Arm"]),
    n_intervention = sum(total_seriousnumatrisk[final_arm_assigned == "Intervention Arm"]),
    .groups = 'drop'
  )

# Conduct meta-analysis to calculate risk ratios (RR)
meta_analysis <- metabin(
  event.e = df_summary$event_intervention, 
  n.e = df_summary$n_intervention,
  event.c = df_summary$event_control, 
  n.c = df_summary$n_control,
  data = df_summary,
  studlab = nctid,
  sm = "RR"  # Risk Ratio
) 

# Generate a forest plot for the meta-analysis
forest(meta_analysis, sortvar = meta_analysis$TE)

# Display summary of the meta-analysis results
summary(meta_analysis)

# Conduct influence analysis to assess impact of individual studies (optional)
#metainf(meta_analysis)
