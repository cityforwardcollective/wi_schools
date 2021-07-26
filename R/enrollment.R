# Enrollment
# Source: https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Enrollment
# Source: https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Enrollment-Private-School

library(tidyverse)
library(readxl)

make_enrollment <- function() {
  
  files <- list.files(path = "imports/e1")
  
  all_enrollment <- map_df(files, function(x) {
    
    x <- paste0("imports/e1/", x)
    
    # Starting in 2020-21, Private Enrollment file format changed to CSV
    # matches public format except for the indicator column,
    # which is CHOICE_IND or CHARTER_IND
    
    if (str_detect(x, ".csv$")) {
      
      temp <- read_csv(x)
      
      # Handle Public CSV
      
      if ("CHARTER_IND" %in% names(temp)) {
        
        temp <- temp %>%
          filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
          mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
                 dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_"),
                 STUDENT_COUNT = as.numeric(STUDENT_COUNT),
                 choice_identifier = "",
                 # 2020-21 changed EL coding
                 GROUP_BY = ifelse(GROUP_BY == "EL Status", "ELL Status", GROUP_BY),
                 GROUP_BY_VALUE = ifelse(GROUP_BY_VALUE == "EL", "ELL/LEP", GROUP_BY_VALUE),
                 cesa = as.numeric(CESA)) %>%
          select(school_year = SCHOOL_YEAR, 
                 dpi_true_id,
                 school_name = SCHOOL_NAME, 
                 agency_type = AGENCY_TYPE,
                 district_name = DISTRICT_NAME, 
                 cesa,
                 group_by = GROUP_BY, 
                 charter_indicator = CHARTER_IND,
                 county = COUNTY, 
                 group_by_value = GROUP_BY_VALUE, 
                 student_count = STUDENT_COUNT, 
                 choice_identifier)
      } else {
        
        # Handle Private CSV
        
        temp <- temp %>%
          filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
          mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
                 DISTRICT_CODE = "0000",
                 dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_"),
                 STUDENT_COUNT = as.numeric(STUDENT_COUNT),
                 charter_indicator = "",
                 # 2020-21 changed EL coding
                 GROUP_BY = ifelse(GROUP_BY == "EL Status", "ELL Status", GROUP_BY),
                 GROUP_BY_VALUE = ifelse(GROUP_BY_VALUE == "EL", "ELL/LEP", GROUP_BY_VALUE),
                 cesa = as.numeric(CESA)) %>%
          select(school_year = SCHOOL_YEAR, 
                 dpi_true_id,
                 school_name = SCHOOL_NAME, 
                 agency_type = AGENCY_TYPE,
                 district_name = DISTRICT_NAME, 
                 cesa,
                 group_by = GROUP_BY, 
                 choice_identifier = CHOICE_IND,
                 county = COUNTY, 
                 group_by_value = GROUP_BY_VALUE, 
                 student_count = STUDENT_COUNT, 
                 charter_indicator)
        
      }
      
      
    } else {
      
      # Handle xl (all private)
      
      temp <- read_xlsx(x, sheet = 3, skip = 2) %>%
        mutate(school_year = paste(as.numeric(Year) - 1, "-", as.numeric(Year) - 2000, sep = ""),
               school_code = str_pad(`Sch Code`, 4, side = "left", pad = "0"),
               district_code = "0000",
               charter_indicator = "No",
               agency_type = "Private school",
               dpi_true_id = paste(district_code, school_code, sep = "_"),
               Grade = str_remove(Grade, "^0"),
               cesa = as.numeric(CESA)) %>%
        select(school_year,
               district_name = `District Name`,
               school_name = School,
               county = County,
               dpi_true_id,
               cesa,
               Grade, Total, charter_indicator, agency_type, choice_identifier = `Choice Identifier`) %>%
        spread(Grade, Total, fill = 0) %>%
        gather("group_by_value", "student_count", 10:27) %>%
        group_by(school_year, dpi_true_id, group_by_value, agency_type, cesa,
                 school_name, district_name, county, charter_indicator, choice_identifier) %>%
        summarise(student_count = sum(student_count, na.rm = TRUE)) %>%
        filter(group_by_value != "<NA>" & !is.na(school_name))
      
      pe_gender <- read_xlsx(x, sheet = 3, skip = 2) %>%
        mutate(school_year = paste(as.numeric(Year) - 1, "-", as.numeric(Year) - 2000, sep = ""),
               school_code = str_pad(`Sch Code`, 4, side = "left", pad = "0"),
               district_code = "0000",
               charter_indicator = "No",
               agency_type = "Private school",
               dpi_true_id = paste(district_code, school_code, sep = "_"),
               Grade = str_remove(Grade, "^0"),
               cesa = as.numeric(CESA)) %>%
        select(school_year,
               district_name = `District Name`,
               school_name = School,
               county = County,
               cesa,
               dpi_true_id,
               Female,
               Male, Total, charter_indicator, agency_type, choice_identifier = `Choice Identifier`) %>%
        pivot_longer(cols = c("Female", "Male"), names_to = "group_by_value", values_to = "student_count") %>%
        group_by(school_year, dpi_true_id, group_by_value, agency_type, cesa,
                 school_name, district_name, county, charter_indicator, choice_identifier) %>%
        summarise(student_count = sum(student_count, na.rm = TRUE)) %>%
        filter(group_by_value != "<NA>" & !is.na(school_name))
      
      
      sum_all <- temp %>%
        group_by(school_year, dpi_true_id, school_name, district_name, cesa,
                 county, charter_indicator, agency_type, choice_identifier) %>%
        summarise(student_count = sum(student_count)) %>%
        mutate(group_by_value = "All Students")
      
      temp <- bind_rows(temp, pe_gender)
      
      temp <- bind_rows(temp, sum_all) %>%
        mutate(group_by = case_when(group_by_value == "All Students" ~ "All Students", 
                                    group_by_value %in% c("Male", "Female") ~ "Gender",
                                    TRUE ~ "Grade Level"))
    }
    
  })
  
  all_enrollment <- all_enrollment %>%
    rename(enrollment_charter_indicator = charter_indicator,
           enrollment_choice_identifier = choice_identifier)
  
  return(all_enrollment)
  
}

all_enrollment <- make_enrollment()
