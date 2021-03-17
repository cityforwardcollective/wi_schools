# Enrollment
# Source: https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Enrollment
# Source: https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Enrollment-Private-School

library(tidyverse)
library(readxl)

make_enrollment <- function() {
  # public file loop ====
  
  public_files <- list.files(path = "imports/enrollment/public")
  
  # Set to NULL because using !exists() doesn't work in for loop
  public_enrollment <- NULL
  
  for (file in public_files) {
    
    filename <- paste("imports/enrollment/public", file, sep = "/")
    
    if (is.null("public_enrollment")) {
      
      public_enrollment <- read_csv(filename) %>%
        filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
        mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
               dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_"),
               STUDENT_COUNT = as.numeric(STUDENT_COUNT),
               choice_identifier = "",
               # 2020-21 changed EL coding
               GROUP_BY = ifelse(GROUP_BY == "EL Status", "ELL Status", GROUP_BY),
               GROUP_BY_VALUE = ifelse(GROUP_BY_VALUE == "EL", "ELL/LEP", GROUP_BY_VALUE)) %>%
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
               choice_identifier)
      
    } else {
      public_enrollment1 <- read_csv(filename) %>%
        filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
        mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
               dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_"),
               STUDENT_COUNT = as.numeric(STUDENT_COUNT),
               choice_identifier = "",
               # 2020-21 changed EL coding
               GROUP_BY = ifelse(GROUP_BY == "EL Status", "ELL Status", GROUP_BY),
               GROUP_BY_VALUE = ifelse(GROUP_BY_VALUE == "EL", "ELL/LEP", GROUP_BY_VALUE)) %>%
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
               choice_identifier)
    }
    
    public_enrollment <- bind_rows(public_enrollment, public_enrollment1)
  }
  
  ## private file loop ====
  
  private_files <- list.files(path = "imports/enrollment/private")
  
  # Set to NULL because using !exists() doesn't work in for loop
  private_enrollment <- NULL
  
  for (file in private_files) {
    
    filename <- paste("imports/enrollment/private", file, sep = "/")
    
    if (is.null("private_enrollment")) {
      
      private_enrollment <- read_xlsx(filename, sheet = 3, skip = 2) %>%
        mutate(school_year = paste(as.numeric(Year) - 1, "-", as.numeric(Year) - 2000, sep = ""),
               school_code = str_pad(`Sch Code`, 4, side = "left", pad = "0"),
               district_code = "0000",
               charter_indicator = "No",
               agency_type = "Private school",
               dpi_true_id = paste(district_code, school_code, sep = "_"),
               Grade = str_remove(Grade, "^0")) %>%
        select(school_year,
               district_name = `District Name`,
               school_name = School,
               county = County,
               dpi_true_id,
               Grade, Total, charter_indicator, agency_type, choice_identifier = `Choice Identifier`) %>%
        spread(Grade, Total, fill = 0) %>%
        gather("group_by_value", "student_count", 9:26) %>%
        group_by(school_year, dpi_true_id, group_by_value, agency_type,
                 school_name, district_name, county, charter_indicator, choice_identifier) %>%
        summarise(student_count = sum(student_count, na.rm = TRUE)) %>%
        filter(group_by_value != "<NA>" & !is.na(school_name))
        
        pe_gender <- read_xlsx(filename, sheet = 3, skip = 2) %>%
          mutate(school_year = paste(as.numeric(Year) - 1, "-", as.numeric(Year) - 2000, sep = ""),
                 school_code = str_pad(`Sch Code`, 4, side = "left", pad = "0"),
                 district_code = "0000",
                 charter_indicator = "No",
                 agency_type = "Private school",
                 dpi_true_id = paste(district_code, school_code, sep = "_"),
                 Grade = str_remove(Grade, "^0")) %>%
          select(school_year,
                 district_name = `District Name`,
                 school_name = School,
                 county = County,
                 dpi_true_id,
                 Female,
                 Male, Total, charter_indicator, agency_type, choice_identifier = `Choice Identifier`) %>%
          pivot_longer(cols = c("Female", "Male"), names_to = "group_by_value", values_to = "student_count") %>%
          group_by(school_year, dpi_true_id, group_by_value, agency_type,
                   school_name, district_name, county, charter_indicator, choice_identifier) %>%
          summarise(student_count = sum(student_count, na.rm = TRUE)) %>%
          filter(group_by_value != "<NA>" & !is.na(school_name))
        
      
      sum_all <- private_enrollment %>%
        group_by(school_year, dpi_true_id, school_name, district_name, county, charter_indicator, agency_type, choice_identifier) %>%
        summarise(student_count = sum(student_count)) %>%
        mutate(group_by_value = "All Students")
      
      private_enrollment <- bind_rows(private_enrollment, pe_gender)
      
      private_enrollment <- bind_rows(private_enrollment, sum_all) %>%
        mutate(group_by = case_when(group_by_value == "All Students" ~ "All Students", 
                                    group_by_value %in% c("Male", "Female") ~ "Gender",
                                    TRUE ~ "Grade Level"))
      
      
    } else {
      
      private_enrollment1 <- read_xlsx(filename, sheet = 3, skip = 2) %>%
        mutate(school_year = paste(as.numeric(Year) - 1, "-", as.numeric(Year) - 2000, sep = ""),
               school_code = str_pad(`Sch Code`, 4, side = "left", pad = "0"),
               district_code = "0000",
               charter_indicator = "No",
               agency_type = "Private school",
               dpi_true_id = paste(district_code, school_code, sep = "_"),
               Grade = str_remove(Grade, "^0")) %>%
        select(school_year,
               district_name = `District Name`,
               school_name = School,
               county = County,
               dpi_true_id,
               Grade, Total, charter_indicator, agency_type, choice_identifier = `Choice Identifier`) %>%
        spread(Grade, Total, fill = 0) %>%
        gather("group_by_value", "student_count", 9:26) %>%
        group_by(school_year, dpi_true_id, group_by_value, agency_type,
                 school_name, district_name, county, charter_indicator, choice_identifier) %>%
        summarise(student_count = sum(student_count, na.rm = TRUE)) %>%
        filter(group_by_value != "<NA>" & !is.na(school_name))
      
      pe_gender <- read_xlsx(filename, sheet = 3, skip = 2) %>%
        mutate(school_year = paste(as.numeric(Year) - 1, "-", as.numeric(Year) - 2000, sep = ""),
               school_code = str_pad(`Sch Code`, 4, side = "left", pad = "0"),
               district_code = "0000",
               charter_indicator = "No",
               agency_type = "Private school",
               dpi_true_id = paste(district_code, school_code, sep = "_"),
               Grade = str_remove(Grade, "^0")) %>%
        select(school_year,
               district_name = `District Name`,
               school_name = School,
               county = County,
               dpi_true_id,
               Female,
               Male, Total, charter_indicator, agency_type, choice_identifier = `Choice Identifier`) %>%
        pivot_longer(cols = c("Female", "Male"), names_to = "group_by_value", values_to = "student_count") %>%
        group_by(school_year, dpi_true_id, group_by_value, agency_type,
                 school_name, district_name, county, charter_indicator, choice_identifier) %>%
        summarise(student_count = sum(student_count, na.rm = TRUE)) %>%
        filter(group_by_value != "<NA>" & !is.na(school_name))
      
      
      
      sum_all <- private_enrollment1 %>%
        group_by(school_year, dpi_true_id, school_name, district_name, county, charter_indicator, agency_type, choice_identifier) %>%
        summarise(student_count = sum(student_count)) %>%
        mutate(group_by_value = "All Students")
      
      private_enrollment1 <- bind_rows(private_enrollment1, pe_gender)
      
      
      private_enrollment1 <- bind_rows(private_enrollment1, sum_all) %>%
        mutate(group_by = case_when(group_by_value == "All Students" ~ "All Students", 
                                    group_by_value %in% c("Male", "Female") ~ "Gender",
                                    TRUE ~ "Grade Level"))
    }
    
    private_enrollment <- bind_rows(private_enrollment, private_enrollment1)
  }
  
  
  
  
  all_enrollment <- bind_rows(public_enrollment, private_enrollment) %>%
    rename(enrollment_charter_indicator = charter_indicator,
           enrollment_choice_identifier = choice_identifier)
  
  return(all_enrollment)
  
}

all_enrollment <- make_enrollment()
