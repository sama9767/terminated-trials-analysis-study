#' @title get_euctr_results
#' 
#' @title get_euctr_results
#' 
#' @description  Retrieves result link if result were posted for EUCTR entry, otherwise returns 'No result found'
#' @param euctr_id Trial registration number for EUCTR trial
#'
#' @return Table identifiers list with EUCTR entry and links to their result section available webpage
#'
#' @example  get_euctr_results("2007-002223-32") ## result ##'https://www.clinicaltrialsregister.eu/ctr-search/trial/2007-002223-32/results'
#' @example  get_euctr_results("2005-003854-80") ## no result found ## NA



library(rvest)

get_euctr_results <- function(euctr_ids) {
  # Create an empty dataframe to store the results
  result_df <- data.frame(euctr_id = character(),
                          result = character(),
                          link = character(),
                          stringsAsFactors = FALSE)
  
  for (euctr_id in euctr_ids) {
    # Check for well-formed input
    if (is.character(euctr_id) && grepl("^[0-9]{4}-[0-9]{6}-[0-9]{2}$", euctr_id)) {
      # Construct the URL
      url <- paste0(
        "https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
        euctr_id
      )
      
      # Read in the HTML document from the URL
      html_doc <- read_html(url)
      
      # Find the "View Results" link in the HTML document
      results_link <- html_doc %>% 
        html_nodes("span.label:contains('Trial results:') + a") %>% 
        html_attr("href")
      
      # Return the results link, if it exists
      if(length(results_link) > 0){
        results_link <- paste0("https://www.clinicaltrialsregister.eu/", results_link)
        result_df <- rbind(result_df, list(euctr_id = euctr_id, result = "result found", link = results_link))
      } else {
        result_df <- rbind(result_df, list(euctr_id = euctr_id, result = "no result found", link = ""))
      }
    }
  }
  
  return(result_df)
}
