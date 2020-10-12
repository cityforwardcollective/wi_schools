# Choice Counts
# Source: https://dpi.wi.gov/sms/choice-programs/data
#
# NOTE: Uses 3rd Friday Head Count

library(tidyverse)

make_choice_counts <- function() {
  choice_names <- read_csv("imports/choice_names.csv")
  
  files <- list.files(path = "imports/choice_counts")
  
  choice_counts <- NULL
  
  for (file in files) {
    
    filename <- paste("imports/choice_counts", file, sep = "/")
    
    cc <- readxl::read_xls(filename, skip = 4) %>%
      filter(!is.na(`...1`)) %>%
      select("school_name" = `School Name`,
             "MPCP_count" = `MPCP Student HC`,
             "RPCP_count" = `RPCP Student HC`,
             "WPCP_count" = `WPCP \nStudent HC`,
             "SNSP_count" = `SNSP Student HC`,
             "ALL_STUDENTS_count" = `Total\nAll\nStudent HC`) %>%
      mutate(school_year = substr(file, 1, 7)) %>%
      left_join(., choice_names)
      
    
    choice_counts <- bind_rows(choice_counts, cc)
  }
  
  mpcp_errors <- choice_counts %>%
    filter(is.na(dpi_true_id) & !is.na(MPCP_count))
  
  if (nrow(mpcp_errors) > 0) {
    
    return(mpcp_errors)
    stop("At least one MPCP school not properly joined.")
    
    
  } else {
    
    return(choice_counts)
    
  }
  
}

# Export objects 

choice_counts <- make_choice_counts()


