library(tidyverse)

make_discipline <- function() {
  
  files <- list.files("imports/discipline/public")
  
  discipline <- map_df(files, function(x) {
    
    filename <- paste("imports/discipline/public", x, sep = "/")
    
    read_csv(filename) %>%
      filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
      modify_at(c("TFS_ENROLLMENT_COUNT", "REMOVAL_COUNT"),
                as.numeric) %>%
      mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
             dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_")) %>%
      select(school_year = SCHOOL_YEAR, 
             dpi_true_id,
             group_by = GROUP_BY, 
             group_by_value = GROUP_BY_VALUE, 
             removal_type_description = REMOVAL_TYPE_DESCRIPTION, 
             tfs_enrollment_count = TFS_ENROLLMENT_COUNT,
             removal_count = REMOVAL_COUNT)
  })
  
  return(discipline)
}


discipline <- make_discipline()

