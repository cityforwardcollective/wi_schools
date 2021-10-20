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
    
    pcp_count <- str_count(str_to_lower(file), "pcp")
    
    filename <- paste("imports/choice_counts", file, sep = "/")
    
    if (pcp_count == 1) {
      year <- str_extract(filename, "\\d{4}-\\d{2,4}")
      
      cc <- readxl::read_xls(filename, skip = 5) %>%
        filter(!is.na(`...1`)) %>%
        select("school_name" = `School Name`,
               "MPCP_count" = `Headcount...5`,
               "ALL_STUDENTS_count" = `Headcount...10`) %>%
        mutate(school_year = year) %>%
        left_join(., choice_names %>% filter(school_year == "2015-16") %>% select(-c(school_year, contains("count"))))
      
      
      choice_counts <- bind_rows(choice_counts, cc)
    } else {
      
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
    
  }
  
  prev_max <- choice_names %>%
    group_by(dpi_true_id) %>%
    filter(school_year == max(school_year)) %>%
    select(school_name, dpi_true_id)
  
  not_in_yet <- choice_counts %>%
    anti_join(., choice_names) %>%
    select(-dpi_true_id) %>%
    left_join(., prev_max) %>%
    bind_rows(., choice_names) %>%
    arrange(school_name, school_year) %>%
    write_csv(., "imports/temp.csv")
  
  mpcp_errors <- not_in_yet %>%
    filter(is.na(dpi_true_id) & !is.na(MPCP_count))
  
  if (nrow(mpcp_errors) > 0) {
    
    warning("At least one MPCP school not properly joined.")
    
    return(mpcp_errors)
    
    
  } else {
    
    return(choice_counts)
    
  }
  
}

# Export objects 

choice_counts <- make_choice_counts()

