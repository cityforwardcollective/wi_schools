# Choice Counts
# Source: https://dpi.wi.gov/parental-education-options/choice-programs/data/mpcp-historical
#
# NOTE: Uses 3rd Friday Head Count

library(tidyverse)
library(readxl)

make_older_choice_counts <- function() {
  files <- list.files(path = "imports/older_enrollment/mpcp")
  
  map_df(files, function(x) {
    

    year <- str_extract(x, "\\d{4}")
    
    if (is.na(year)) {
      year <- str_extract(x, "\\d{2}")
    }
    
    year <- str_extract(year, "\\d{2}$")
    year <- as.numeric(paste0("20", year))
    school_year <- paste(year, str_pad(year + 1 - 2000, width = 2, side = "left", pad = 0), sep = "-")
    
    filename <- paste("imports/older_enrollment/mpcp", x, sep = "/")
    
    data <- readxl::read_excel(filename, skip = 5)
    
    headcount <- which(names(data) == "School Name") + 1
    
    data %>%
      filter(!is.na(`...1`) & !str_detect(`School Name`, "Total")) %>%
      select("school_name" = `School Name`,
             "MPCP_count" = names(data)[headcount]) %>%
      mutate(school_year = school_year)
  })
}

# Export objects 

older_mpcp_counts <- make_older_choice_counts() %>%
  group_by(school_year) %>%
  summarise(total = sum(MPCP_count, na.rm = TRUE))
