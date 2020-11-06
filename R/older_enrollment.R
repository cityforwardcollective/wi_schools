# Older Enrollment
# Source: https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Enrollment

library(tidyverse)

make_older_enrollment <- function() {
  
  public_files <- list.files(path = "imports/older_enrollment/public")
  
  map_df(public_files, function(x) {
    
    filename <- paste("imports/older_enrollment/public", x, sep = "/")
    
    public_enrollment <- read_csv(filename) %>%
      filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
      mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
             dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_"),
             STUDENT_COUNT = as.numeric(STUDENT_COUNT),
             choice_identifier = "") %>%
      select(school_year = SCHOOL_YEAR, 
             dpi_true_id,
             school_name = SCHOOL_NAME, 
             agency_type = AGENCY_TYPE,
             district_name = DISTRICT_NAME, 
             group_by = GROUP_BY, 
             charter_indicator = CHARTER_IND,
             county = COUNTY, 
             group_by_value = GROUP_BY_VALUE, 
             student_count = STUDENT_COUNT, 
             choice_identifier) %>%
      filter(group_by_value == "All Students")
  })
}
  
  
older_enrollment <- make_older_enrollment()
  