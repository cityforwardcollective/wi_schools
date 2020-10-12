# OPEN ENROLLMENT ====
# Source: https://dpi.wi.gov/open-enrollment/data/aid-adjustments

library(tidyverse)
library(readxl)

make_open_enrollment <- function() {
  oe_files <- list.files(path = "imports/open_enrollment")
  
  open_enrollment <- NULL
  
  for (file in oe_files) {
    
    filename <- paste("imports/open_enrollment", file, sep = "/")
    
    
    if (is.null("open_enrollment")) {
      
      open_enrollment <- read_xlsx(filename, skip = 4)
      
      names(open_enrollment) <- tolower(names(open_enrollment))
      names(open_enrollment) <- str_replace_all(string = names(open_enrollment), " ", "_")
      
      open_enrollment <- open_enrollment %>%
        filter(!is.na(year)) %>%
        rename("district_code" = dist_no) %>% 
        mutate(school_year = str_sub(file, 1, 7)) %>%
        modify_at(4:9, as.numeric)
      
    } else {
      
      oe <- read_xlsx(filename, skip = 4)
      
      names(oe) <- tolower(names(oe))
      names(oe) <- str_replace_all(string = names(oe), " ", "_")
      
      oe <- oe %>%
        filter(!is.na(year)) %>%
        rename("district_code" = dist_no) %>% 
        mutate(school_year = str_sub(file, 1, 7)) %>%
        modify_at(4:9, as.numeric)
    }
    open_enrollment <- bind_rows(open_enrollment, oe) 
  }
  
  # Open Enrollment Table
  
  open_enrollment <- open_enrollment %>%
    select(-c(1, 11))
  
  return(open_enrollment)
}

open_enrollment <- make_open_enrollment()

