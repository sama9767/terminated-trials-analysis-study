# Analyze termination reasons

library(dplyr)

# Read the extraction form file
extraction_form <- read.delim(here::here("data", "manual", "2024-04-25_081753-form_1-refset_5-final.tsv"), header = TRUE)

# Get analyzed terminated trials data
terminated_combined <- read.csv(here::here("data", "processed", "terminated_combined_1.csv"))

# Categorize the reasons for termination based on provided manual codebook
# Assign categories: scientific, non-scientific, reason not provided, and other
extraction_form_updated <- 
  extraction_form |>
  mutate(reason_category = case_when(
    primary_reason %in% c("1a", "1b", "1c", "1d", "1e") ~ "scientific_reason",      # Scientific reasons
    primary_reason %in% c("2a", "2b", "2c", "2d", "2e") ~ "non_scientific_reason",  # Non-scientific reasons
    primary_reason == "3a" ~ "reason_not_provided",                                 # No reason provided
    primary_reason == "3b" ~ "other",                                               # Other/unspecified reason
  ),
  # Adding a new column for detailed reasons
  primary_reason_recoded = case_when(
    primary_reason == "1a" ~ "Evidence of harm",
    primary_reason == "1b" ~ "Evidence of benefit",
    primary_reason == "1c" ~ "Evidence of futility",
    primary_reason == "1d" ~ "External evidence",
    primary_reason == "1e" ~ "Internal evidence (unspecified)",
    primary_reason == "2a" ~ "Low accrual rate",
    primary_reason == "2b" ~ "Lack of funding",
    primary_reason == "2c" ~ "Principal investigator departure",
    primary_reason == "2d" ~ "Lack of investigational product",
    primary_reason == "2e" ~ "Administrative, logistical, or technical issues",
    primary_reason == "3a" ~ "Reason not provided",
    primary_reason == "3b" ~ "Other/unspecified reason",
    TRUE ~ "NA"
  )) |>
  mutate(
    combination_reason_category = case_when(
      # Both scientific and non-scientific reasons
      primary_reason %in% c("1a", "1b", "1c", "1d", "1e") & 
        rowSums(across(starts_with("secondary_reasons_2")) == "1", na.rm = TRUE) > 0 ~ "both_scientific_and_non_scientific",
      
      primary_reason %in% c("2a", "2b", "2c", "2d", "2e") & 
        rowSums(across(starts_with("secondary_reasons_1")) == "1", na.rm = TRUE) > 0 ~ "both_scientific_and_non_scientific",
      
      # Only scientific reasons
      primary_reason %in% c("1a", "1b", "1c", "1d", "1e") ~ "scientific_reason",
      
      # Only non-scientific reasons
      primary_reason %in% c("2a", "2b", "2c", "2d", "2e") ~ "non_scientific_reason"
      
    )
  )

# Count the number of trials for each reason category
reason_category_count <- table(extraction_form_updated$reason_category)
combined_reason_category_count <- table(extraction_form_updated$combination_reason_category)



# Get final data with reason for termination categorty 
terminated_combined <- 
  terminated_combined |>
  left_join(
    extraction_form_updated |> 
      select(nctid, reason_for_termination, primary_reason, reason_category, primary_reason_recoded,combination_reason_category ),
    by = "nctid"
  ) |>
  select(-reason_for_termination.x, -primary_reason) |>
  rename(reason_for_termination = reason_for_termination.y)

termination_reason_table <- 
  furniture::table1(terminated_combined, 
                    primary_reason_recoded,
                    combination_reason_category,
                    splitby = "source",
                    na.rm = F)

# Filter trials terminated for non-scientific reasons
trials_non_scientific <- 
  extraction_form_updated |>
  filter(reason_category == "non_scientific_reason")


# Analyze how patients were included in these trials terminated due to scientific and non-scientific reason 
# and how long these trials were running (e.g. average trial days and actual enrollment).
termination_summary <- terminated_combined %>%
 filter(!is.na(reason_category)) %>%
 group_by(reason_category) %>%
   summarise(
    n_trials = n(),
    avg_enrollment = round(mean(actual_enrollment, na.rm = TRUE)),
    median_enrollment = median(actual_enrollment, na.rm = TRUE),
    avg_duration_days = round(mean(trial_days, na.rm = TRUE)),
    median_duration_days = median(trial_days, na.rm = TRUE)
         )


#write.csv(terminated_combined, file = here::here("data", "processed", "terminated_combined_2.csv"))
#write.csv(extraction_form_updated, here::here("data","processed", "extraction_form_updated.csv"), row.names = FALSE)
#write.csv(trials_non_scientific, here::here("data","processed", "trials_non_scientific.csv"), row.names = FALSE)
