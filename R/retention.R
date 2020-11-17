library(tidyverse)

make_retention <- function() {
  
  files <- list.files("imports/retention/public")
  
  attendance <- map_df(files, function(x) {
    
    filename <- paste("imports/retention/public", x, sep = "/")
    
    d <- read_csv(filename) 
    
    if ("GROUP_BY_CATEGORY" %in% names(d)) {
      names(d)[which(names(d) == "GROUP_BY_CATEGORY")] <- "GROUP_BY_VALUE"
    }
    
    d %>%
      filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
      modify_at(c("NUMBER_COMPLETED_SCHOOL_TERM", "NUMBER_OF_RETENTIONS"),
                as.numeric) %>%
      mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
             dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_")) %>%
      select(school_year = SCHOOL_YEAR, 
             dpi_true_id,
             group_by = GROUP_BY, 
             group_by_value = GROUP_BY_VALUE, 
             number_completed_school_term = NUMBER_COMPLETED_SCHOOL_TERM,
             number_of_retentions = NUMBER_OF_RETENTIONS)
  })
  
  return(attendance)
}


retention <- make_retention()

