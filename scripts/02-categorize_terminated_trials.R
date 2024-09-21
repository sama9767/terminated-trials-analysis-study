# Analyzing terminated trials using two datasets from German and Californian UMCs

library(dplyr)

# Read the extraction form file
extraction_form <- read.delim(here::here("data", "manual", "2024-04-25_081753-form_1-refset_5-final.tsv"), header = TRUE)

# Get analyzed terminated trials data
cthist_terminated <- readRDS(here::here("data", "processed", "cthist_terminated_1.rds"))

# Categorize the reasons for termination based on provided manual codebook
# Assign categories: scientific, non-scientific, reason not provided, and other
extraction_form_updated <- 
  extraction_form |>
  mutate(reason_category = case_when(
    primary_reason %in% c("1a", "1b", "1c", "1d", "1e") ~ "scientific_reason",      # Scientific reasons
    primary_reason %in% c("2a", "2b", "2c", "2d", "2e") ~ "non_scientific_reason",  # Non-scientific reasons
    primary_reason == "3a" ~ "reason_not_provided",                                 # No reason provided
    primary_reason == "3b" ~ "other",                                               # Other/unspecified reason
    TRUE ~ "unknown"                                                                # Fallback for unknown reasons
  )) |>
  select(nctid, reason_for_termination, primary_reason, reason_category)

# Count the number of trials for each reason category
reason_category_count <- table(extraction_form_updated$reason_category)
print(reason_category_count)  # Display the counts for each category

# Filter trials terminated for non-scientific reasons
trials_non_scientific <- 
  extraction_form_updated |>
 filter(reason_category == "non_scientific_reason")

cthist_terminated <- 
  cthist_terminated |>
  left_join(extraction_form_updated, by = "nctid") |>
  select(-reason_for_termination.x, -primary_reason) |>
  rename(reason_for_termination = reason_for_termination.y)

saveRDS(cthist_terminated, file = here::here("data", "processed", "cthist_terminated_2.rds"))
write.csv(extraction_form_updated, here::here("data","processed", "extraction_form_updated.csv"), row.names = FALSE)
write.csv(trials_non_scientific, here::here("data","processed", "trials_non_scientific.csv"), row.names = FALSE)