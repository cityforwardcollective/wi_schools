# HIGH SCHOOL COMPLETION ====
# Source: https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=hs-completion
# 
# Note: Private School Completion Not Comparable because 
# no cohort information provided

public_grad_files <- list.files(path = "imports/graduation/public")

# Set to NULL because using !exists() doesn't work in for loop
public_graduation <- NULL

for (file in public_grad_files) {
  
  filename <- paste("imports/graduation/public", file, sep = "/")
  
  if (is.null("public_graduation")) {
    
    public_graduation <- read_csv(filename) %>%
      filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
      mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
             DISTRICT_CODE = str_pad(DISTRICT_CODE, 4, side = "left", pad = "0"),
             dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_"),
             STUDENT_COUNT = as.numeric(STUDENT_COUNT)) %>%
      select(school_year = SCHOOL_YEAR, 
             dpi_true_id, 
             district_name = DISTRICT_NAME, 
             group_by = GROUP_BY, 
             cohort = COHORT,
             completion_status = COMPLETION_STATUS,
             timeframe = TIMEFRAME,
             group_by_value = GROUP_BY_VALUE, 
             student_count = STUDENT_COUNT,
             cohort_count = COHORT_COUNT)
    
  } else {
    public_graduation1 <- read_csv(filename) %>%
      filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
      mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
             DISTRICT_CODE = str_pad(DISTRICT_CODE, 4, side = "left", pad = "0"),
             dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_"),
             STUDENT_COUNT = as.numeric(STUDENT_COUNT)) %>%
      select(school_year = SCHOOL_YEAR, 
             dpi_true_id, 
             district_name = DISTRICT_NAME, 
             group_by = GROUP_BY, 
             cohort = COHORT,
             completion_status = COMPLETION_STATUS,
             timeframe = TIMEFRAME,
             group_by_value = GROUP_BY_VALUE, 
             student_count = STUDENT_COUNT,
             cohort_count = COHORT_COUNT)
  }
  
  public_graduation <- bind_rows(public_graduation, public_graduation1)
}
