#' @title adverse_events
#'
#' @description Retrieves adverse events section for a EUCTR entry with result section 
#'
#' @param trn A character vector containing a EUCTR trial registry
#'     number with result section 
#'
#' @return adverse event table of the trial 
# Note  will not fetch if result section is an attachment or summary report link
# 
#' @example  adverse_events("2008-006649-18") 


library(rvest)
library(xml2)


adverse_events <- function(trn) {
  
  # Construct the URL
  url <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/trial/", trn, "/results#adverseEventsSection")
  
  # Read the HTML content from the URL
  html <- read_html(url)
  
  # Extract the Adverse events section table using its IDx
  adverse_events_table <- html %>%
    html_nodes("#adverseEventsSection table") %>%
    html_table(fill = TRUE) # Parses tables into data frames
  
  # If the Adverse events section table is found, return the table
  if(length(adverse_events_table) > 0) {
    return(adverse_events_table)
  } else {
    return("No result found")
  }
}
