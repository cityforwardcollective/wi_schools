library(tidyverse)
library(DBI)
library(RSQLite)
library(readxl)


# CHAPTER 220 ====

school_year <- c("2015-16", "2016-17", "2017-18", "2018-19")
mke_out_sept <- c(1266 , 1038, 842, 672)

chapter_220 <- tibble(school_year, mke_out_sept)

# OPEN ENROLLMENT ====

oe_files <- list.files(path = "imports/open_enrollment")

open_enrollment <- NULL

for(file in oe_files) {
  filename <- paste("imports/open_enrollment", file, sep = "/")
  
  if(is.null("open_enrollment")) {
    
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

open_enrollment <- open_enrollment %>%
  select(-c(1, 11))

# GRADUATION ====

public_grad_files <- list.files(path = "imports/graduation/public")

# Set to NULL because using !exists() doesn't work in for loop
public_graduation <- NULL

for(file in public_grad_files) {
  
  filename <- paste("imports/graduation/public", file, sep = "/")
  
  if(is.null("public_graduation")) {
    
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

# public file loop ====

public_files <- list.files(path = "imports/enrollment/public")

# Set to NULL because using !exists() doesn't work in for loop
public_enrollment <- NULL

for(file in public_files) {
  
  filename <- paste("imports/enrollment/public", file, sep = "/")
  
  if(is.null("public_enrollment")) {
    
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
             choice_identifier)
    
  } else {
    public_enrollment1 <- read_csv(filename) %>%
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
             choice_identifier)
  }
  
  public_enrollment <- bind_rows(public_enrollment, public_enrollment1)
}

# private file loop

private_files <- list.files(path = "imports/enrollment/private")

# Set to NULL because using !exists() doesn't work in for loop
private_enrollment <- NULL

for(file in private_files) {
  
  filename <- paste("imports/enrollment/private", file, sep = "/")
  
  if(is.null("private_enrollment")) {
    
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
    
    sum_all <- private_enrollment %>%
      group_by(school_year, dpi_true_id, school_name, district_name, county, charter_indicator, agency_type, choice_identifier) %>%
      summarise(student_count = sum(student_count)) %>%
      mutate(group_by_value = "All Students")
    
    private_enrollment <- bind_rows(private_enrollment, sum_all) %>%
      mutate(group_by = ifelse(group_by_value == "All Students", "All Students", "Grade Level"))
    
    
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
    
    sum_all <- private_enrollment1 %>%
      group_by(school_year, dpi_true_id, school_name, district_name, county, charter_indicator, agency_type, choice_identifier) %>%
      summarise(student_count = sum(student_count)) %>%
      mutate(group_by_value = "All Students")
    
    private_enrollment1 <- bind_rows(private_enrollment1, sum_all) %>%
      mutate(group_by = ifelse(group_by_value == "All Students", "All Students", "Grade Level"))
  }
  
  private_enrollment <- bind_rows(private_enrollment, private_enrollment1)
}




all_enrollment <- bind_rows(public_enrollment, private_enrollment)

only_enrollment <- all_enrollment %>%
  mutate(student_count = as.numeric(student_count)) %>%
  select(school_year, dpi_true_id, group_by, group_by_value, student_count)


instrumentality <- c("3619_0162",
                     "3619_0413",
                     "3619_0334",
                     "3619_0165",
                     "3619_0398")

partnership <- c("3619_1063",
                 "3619_0432",
                 "3619_0410",
                 "3619_0296",
                 "3619_1072",
                 "3619_1074",
                 "3619_1086")

unique_schools_ly <- all_enrollment %>%
  modify_at("school_year", factor, ordered = TRUE) %>%
  group_by(dpi_true_id) %>%
  summarise(last_year_open = max(school_year))

unique_schools <- unique_schools_ly %>%
  left_join(., all_enrollment, by = c("dpi_true_id", "last_year_open" = "school_year")) %>%
  mutate(charter_indicator = ifelse(charter_indicator == "Yes", 1, 0),
         choice_indicator = ifelse(choice_identifier == "CHC", 1, 0)) %>%
  select(dpi_true_id, school_name, agency_type, district_name, county, choice_indicator, charter_indicator,
         last_year_open) %>%
  unique() %>%
  mutate(choice_indicator = ifelse(is.na(choice_indicator), 0, choice_indicator),
         accurate_agency_type = ifelse(agency_type == "Non District Charter Schools", "2r/2x Charter",
                                       ifelse(agency_type == "Private school", "Private",
                                              ifelse(dpi_true_id %in% instrumentality, "Instrumentality Charter",
                                                     ifelse(agency_type == "Public school" & charter_indicator == 1, "Non-Instrumentality Charter",
                                                            "Traditional Public")))),
         broad_agency_type = ifelse(accurate_agency_type %in% c("2r/2x Charter", "Non-Instrumentality Charter"), "Independent Charter",
                                    ifelse(accurate_agency_type == "Private", "Private",
                                           "District-Run")))


# Create vector of files in the folder

files <- list.files(path = "./imports/report_cards")

# Initialize objects for use in for loop

rc <- rc_renamed <- rc_other <- NULL


for (file in files) {
  
  # Create full file paths outside of if-else statements
  
  fileNames <- paste("imports/report_cards", file, sep = "/")
  
  # First conditional used if the initial tibble
  # has not been populated with any data.
  # Otherwise would overwrite with each loop.
  
  if(is.null(rc_renamed)) {
    
    rc <- read_xlsx(fileNames, sheet = "Data")
    
    rc_renamed <- rc %>%
      
      # Using `mutate` renames columns and populates with extant data,
      # while allowing for `ifelse` where certain fields don't exist
      # in all original files.  `select` would only allow for renaming.
      
      mutate(school_year = `School Year`,
             district_code = `District Code`,
             school_code = `School Code`,
             district_name = `District Name`,
             school_name = `School Name`,
             overall_score = `Overall Accountability Score`,
             overall_rating = `Overall Accountability Rating`,
             lowest_grade = `Lowest Grade in the School`,
             highest_grade = `Highest Grade in the School`,
             grade_band = `Grade Band for Comparison Schools`,
             school_type = `School Type`,
             school_enrollment = `School Enrollment`,
             district_enrollment = `District Enrollment`,
             per_am_in = `Percent American Indian or Alaskan Native`,
             per_asian = `Percent Asian`,
             per_b_aa = `Percent Black or African American`,
             per_hisp_lat = `Percent Hispanic/Latino`,
             per_nh_opi = `Percent Native Hawaiian or Other Pacific Islander`,
             per_white = `Percent White`,
             per_tom = `Percent Two or More Races`,
             per_swd = `Percent Students with Disabilities`,
             per_ed = `Percent Economically Disadvantaged`,
             per_lep = `Percent Limited English Proficient`,
             per_choice = `Percent School Choice Program`,
             per_open = `Percent Open Enrollment`,
             sch_ach = `School Student Achievement Score`,
             sch_ela_ach = `School ELA Achievement Score`,
             sch_math_ach = `School Mathematics Achievement Score`,
             sch_growth = `School Student Growth Score`,
             sch_ela_growth = `School ELA Growth Score`,
             sch_math_growth = `School Mathematics Growth Score`,
             sch_cg = `School Closing Gaps Score`,
             sch_ela_cg = `School ELA Gap Score`,
             sch_math_cg = `School Mathematics Gap Score`,
             
             # These fields don't exist in all files, so will throw an error
             # without using conditional.
             
             sch_4y_grad_gap = if("School 4 Year Graduation Gap Score" %in% colnames(rc)) {
               `School 4 Year Graduation Gap Score`} else {NA},
             sch_6y_grad_gap = if("School 6 Year Graduation Gap Score" %in% colnames(rc)) {
               `School 6 Year Graduation Gap Score`} else {NA},
             sch_grad_gap = `School Graduation Gap Score`,
             sch_ot = `School On-Track and Postsecondary Readiness Score`,
             sch_grad_rate = `School Graduation Rate Score`,
             sch_att_rate = `School Attendance Rate Score`,
             sch_3rd_ela = `School Third-Grade ELA Achievement Score`,
             sch_8th_math = `School Eighth-Grade Mathematics Achievement Score`,
             ach_weight = if("Score weighting Achievement Priority Area" %in% colnames(rc)) {
               as.numeric(`Score weighting Achievement Priority Area`)} else {NA},
             growth_weight = if("Score weighting Growth Priority Area" %in% colnames(rc)) {
               as.numeric(`Score weighting Growth Priority Area`)} else {NA},
             cg_weight = if("Score weighting Closing Gaps Priority Area" %in% colnames(rc)) {
               as.numeric(`Score weighting Closing Gaps Priority Area`)} else {NA},
             ot_weight = if("Score weighting Ontrack Priority Area" %in% colnames(rc)) {
               as.numeric(`Score weighting Ontrack Priority Area`)} else {NA},
             locale_description = `Locale description`,
             city = City,
             report_card_type = if("Report Card Type" %in% colnames(rc)) {
               `Report Card Type`} else {NA},
             district_code = gsub("NA", 0000, district_code),
             district_code = str_pad(district_code, 4, side = "left", pad = 0),
             school_code = str_replace(school_code, "[:alpha:]", ""),
             school_code = str_pad(school_code, 4, side = "left", pad = 0),
             dpi_true_id = paste(district_code, school_code, sep = "_"),
             overall_score = round(as.numeric(overall_score), 1),
             school_enrollment = as.numeric(school_enrollment),
             district_enrollment = as.numeric(district_enrollment),
             cg_redacted = ifelse(str_detect(sch_cg, "\\*"), 1, 0)) %>%
      mutate_at(vars("per_am_in":"per_open"), funs(round(as.numeric(.), 2))) %>%
      mutate_at(vars("sch_ach":"sch_8th_math"), funs(round(as.numeric(.), 1))) %>%
      
      # Since `mutate` was used above, columns were created instead of
      # being renamed.  Using select drops all original columns and leaves
      # just those we are interested in; also, using column names facilitates
      # functionality across files where column indices might differ.
      
      select(school_year,
             dpi_true_id,
             district_name,
             school_name,
             overall_score,
             overall_rating,
             lowest_grade,
             highest_grade,
             grade_band,
             school_type,
             school_enrollment,
             district_enrollment,
             per_am_in,
             per_asian,
             per_b_aa,
             per_hisp_lat,
             per_nh_opi,
             per_white,
             per_tom,
             per_swd,
             per_ed,
             per_lep,
             per_choice,
             per_open,
             sch_ach,
             sch_ela_ach,
             sch_math_ach,
             sch_growth,
             sch_ela_growth,
             sch_math_growth,
             sch_cg,
             sch_ela_cg,
             sch_math_cg,
             sch_4y_grad_gap,
             sch_6y_grad_gap,
             sch_grad_gap,
             sch_ot,
             sch_grad_rate,
             sch_att_rate,
             sch_3rd_ela,
             sch_8th_math,
             ach_weight,
             growth_weight,
             cg_weight,
             ot_weight,
             locale_description,
             city,
             report_card_type,
             cg_redacted)
  }
  
  # Second conditional used for following loops.
  # rc_other created then appended to rc_renamed.
  # rc_renamed grows with every loop.
  
  else {
    rc <- read_xlsx(fileNames, sheet = "Data")
    
    rc_other <- rc %>%
      mutate(school_year = `School Year`,
             district_code = `District Code`,
             school_code = `School Code`,
             district_name = `District Name`,
             school_name = `School Name`,
             overall_score = `Overall Accountability Score`,
             overall_rating = `Overall Accountability Rating`,
             lowest_grade = `Lowest Grade in the School`,
             highest_grade = `Highest Grade in the School`,
             grade_band = `Grade Band for Comparison Schools`,
             school_type = `School Type`,
             school_enrollment = `School Enrollment`,
             district_enrollment = `District Enrollment`,
             per_am_in = `Percent American Indian or Alaskan Native`,
             per_asian = `Percent Asian`,
             per_b_aa = `Percent Black or African American`,
             per_hisp_lat = `Percent Hispanic/Latino`,
             per_nh_opi = `Percent Native Hawaiian or Other Pacific Islander`,
             per_white = `Percent White`,
             per_tom = `Percent Two or More Races`,
             per_swd = `Percent Students with Disabilities`,
             per_ed = `Percent Economically Disadvantaged`,
             per_lep = `Percent Limited English Proficient`,
             per_choice = `Percent School Choice Program`,
             per_open = `Percent Open Enrollment`,
             sch_ach = `School Student Achievement Score`,
             sch_ela_ach = `School ELA Achievement Score`,
             sch_math_ach = `School Mathematics Achievement Score`,
             sch_growth = `School Student Growth Score`,
             sch_ela_growth = `School ELA Growth Score`,
             sch_math_growth = `School Mathematics Growth Score`,
             sch_cg = `School Closing Gaps Score`,
             sch_ela_cg = `School ELA Gap Score`,
             sch_math_cg = `School Mathematics Gap Score`,
             sch_4y_grad_gap = if("School 4 Year Graduation Gap Score" %in% colnames(rc)) {
               `School 4 Year Graduation Gap Score`} else {NA},
             sch_6y_grad_gap = if("School 6 Year Graduation Gap Score" %in% colnames(rc)) {
               `School 6 Year Graduation Gap Score`} else {NA},
             sch_grad_gap = `School Graduation Gap Score`,
             sch_ot = `School On-Track and Postsecondary Readiness Score`,
             sch_grad_rate = `School Graduation Rate Score`,
             sch_att_rate = `School Attendance Rate Score`,
             sch_3rd_ela = `School Third-Grade ELA Achievement Score`,
             sch_8th_math = `School Eighth-Grade Mathematics Achievement Score`,
             ach_weight = if("Score weighting Achievement Priority Area" %in% colnames(rc)) {
               as.numeric(`Score weighting Achievement Priority Area`)} else {NA},
             growth_weight = if("Score weighting Growth Priority Area" %in% colnames(rc)) {
               as.numeric(`Score weighting Growth Priority Area`)} else {NA},
             cg_weight = if("Score weighting Closing Gaps Priority Area" %in% colnames(rc)) {
               as.numeric(`Score weighting Closing Gaps Priority Area`)} else {NA},
             ot_weight = if("Score weighting Ontrack Priority Area" %in% colnames(rc)) {
               as.numeric(`Score weighting Ontrack Priority Area`)} else {NA},
             locale_description = `Locale description`,
             city = City,
             report_card_type = if("Report Card Type" %in% colnames(rc)) {
               `Report Card Type`} else {NA},
             district_code = gsub("NA", 0000, district_code),
             district_code = str_pad(district_code, 4, side = "left", pad = 0),
             school_code = str_replace(school_code, "[:alpha:]", ""),
             school_code = str_pad(school_code, 4, side = "left", pad = 0),
             dpi_true_id = paste(district_code, school_code, sep = "_"),
             overall_score = round(as.numeric(overall_score), 1),
             school_enrollment = as.numeric(school_enrollment),
             district_enrollment = as.numeric(district_enrollment),
             cg_redacted = ifelse(str_detect(sch_cg, "\\*"), 1, 0)) %>%
      mutate_at(vars("per_am_in":"per_open"), funs(round(as.numeric(.), 2))) %>%
      mutate_at(vars("sch_ach":"sch_8th_math"), funs(round(as.numeric(.), 1))) %>%
      select(school_year,
             dpi_true_id,
             district_name,
             school_name,
             overall_score,
             overall_rating,
             lowest_grade,
             highest_grade,
             grade_band,
             school_type,
             school_enrollment,
             district_enrollment,
             per_am_in,
             per_asian,
             per_b_aa,
             per_hisp_lat,
             per_nh_opi,
             per_white,
             per_tom,
             per_swd,
             per_ed,
             per_lep,
             per_choice,
             per_open,
             sch_ach,
             sch_ela_ach,
             sch_math_ach,
             sch_growth,
             sch_ela_growth,
             sch_math_growth,
             sch_cg,
             sch_ela_cg,
             sch_math_cg,
             sch_4y_grad_gap,
             sch_6y_grad_gap,
             sch_grad_gap,
             sch_ot,
             sch_grad_rate,
             sch_att_rate,
             sch_3rd_ela,
             sch_8th_math,
             ach_weight,
             growth_weight,
             cg_weight,
             ot_weight,
             locale_description,
             city,
             report_card_type,
             cg_redacted)
    
  }
  rc_renamed <- bind_rows(rc_renamed, rc_other)
}

# Add variable to indicate when two report cards exist for
# a private school in a year.

mult_rc <- rc_renamed %>%
  group_by(dpi_true_id, school_year) %>%
  summarise(number = n()) %>%
  mutate(has_2_rc = ifelse(number == 2, 1, 0)) %>%
  select(-number)

rc_renamed <- left_join(rc_renamed, mult_rc, by = c("dpi_true_id", "school_year"))

# remove school year filter, add "last year open" field

rc1 <- rc_renamed %>%
  modify_at("school_year", factor, ordered = TRUE)  %>%
  group_by(dpi_true_id) %>%
  summarize(last_year = max(school_year)) %>%
  ungroup() %>%
  left_join(., rc_renamed %>%
              select(locale_description, city, dpi_true_id, school_year),
            by = c("dpi_true_id", "last_year" = "school_year")) %>%
  unique() %>%
  select(-last_year)

# NOTE: locale description is "NA" in source files
# for private schools

unique_schools <- left_join(unique_schools, rc1, by = "dpi_true_id")

# Forward Exam ====

files <- list.files(path = "./imports/wsas/private")

# Set to NULL because using !exists() doesn't work in for loop
choice_forward <- choice1_forward <- NULL

fix_format <- function(x) {
  x <- select(x, -contains("Required"))
  colnames(x) <- c("school_year",
                   "MPCP",
                   "RPCP",
                   "WPCP",
                   "grade",
                   "enrollment",
                   "parent_opt_out",
                   "not_tested",
                   "Below Basic",
                   "Basic",
                   "Proficient",
                   "Advanced",
                   "dpi_true_id",
                   "school_name")
  
  x <- x %>%
    mutate_at(9:12, as.integer) %>%
    mutate(group_count = `Below Basic` + Basic + Proficient + Advanced) %>%
    gather(key = "test_result", value = "student_count", `Below Basic`:Advanced)
}

for (file in files) {
  
  filename <- paste("imports/wsas/private", file, sep = "/")
  
  if(length(choice_forward) == 0) {
    raw_choice <- read_xlsx(filename, "Opt Out Not Included", skip = 1, col_names = TRUE)
    
    raw_choice <- raw_choice %>% select(-contains("%")) %>%
      mutate(dpi_true_id = str_replace_all(str_extract(raw_choice$`School Name and Number`, "\\(.*\\)$"), "\\(|\\)", ""),
             dpi_true_id = ifelse(is.na(dpi_true_id), dpi_true_id, paste("0000", dpi_true_id, sep = "_")),
             school_name = ifelse(!str_detect(raw_choice$`School Name and Number`, "-|\\("), raw_choice$`School Name and Number`,
                                  str_replace(str_extract(raw_choice$`School Name and Number`, ".*-|.*\\("), "-|\\(", "")))
    
    choice_ela <- raw_choice[, c(1, 3:14, 39:40)]
    choice_math <- raw_choice[, c(1, 3:6, 15:22, 39:40)]
    choice_ss <- raw_choice[, c(1, 3:6, 23:30, 39:40)]
    choice_science <- raw_choice[, c(1, 3:6, 31:40)]
    
    choice_subjects <- list(ELA = choice_ela, Mathematics = choice_math, Science = choice_science, "Social Studies" = choice_ss)
    
    choice_gathered <- map(choice_subjects, fix_format)
    
    choice_gathered <- bind_rows(choice_gathered, .id = "test_subject")
    choice_forward <- choice_gathered %>%
      mutate(test_group = ifelse(grade %in% 3:8, "Forward",
                                 ifelse(grade == 10 & test_subject == "social studies", "Forward",
                                        ifelse(grade %in% 9:10, "Aspire",
                                               ifelse(grade == 11, "ACT",
                                                      "ERROR")))),
             opt_outs_excluded = enrollment - parent_opt_out,
             group_by = "All Students",
             group_by_value = "All Students") %>%
      modify_at("student_count", as.integer) %>%
      filter(grade != "Total" & !str_detect(school_name, "Choice Program")) %>%
      select(-c(enrollment))
  } else {
    
  raw_choice1 <- read_xlsx(filename, "Opt Out Not Included", skip = 1, col_names = TRUE)
  
  raw_choice1 <- raw_choice1 %>% select(-contains("%")) %>%
    mutate(dpi_true_id = str_replace_all(str_extract(raw_choice1$`School Name and Number`, "\\(.*\\)$"), "\\(|\\)", ""),
           dpi_true_id = ifelse(is.na(dpi_true_id), dpi_true_id, paste("0000", dpi_true_id, sep = "_")),
           school_name = ifelse(!str_detect(raw_choice1$`School Name and Number`, "-|\\("), raw_choice1$`School Name and Number`,
                                str_replace(str_extract(raw_choice1$`School Name and Number`, ".*-|.*\\("), "-|\\(", "")))
  
  choice1_ela <- raw_choice1[, c(1, 3:14, 39:40)]
  choice1_math <- raw_choice1[, c(1, 3:6, 15:22, 39:40)]
  choice1_ss <- raw_choice1[, c(1, 3:6, 23:30, 39:40)]
  choice1_science <- raw_choice1[, c(1, 3:6, 31:40)]
  
  choice1_subjects <- list(ELA = choice1_ela, Mathematics = choice1_math, Science = choice1_science, "Social Studies" = choice1_ss)
  
  choice1_gathered <- map(choice1_subjects, fix_format)
  
  choice1_gathered <- bind_rows(choice1_gathered, .id = "test_subject")
  choice1_forward <- choice1_gathered %>%
    mutate(test_group = ifelse(grade %in% 3:8, "Forward",
                               ifelse(grade == 10 & test_subject == "social studies", "Forward",
                                      ifelse(grade %in% 9:10, "Aspire",
                                             ifelse(grade == 11, "ACT",
                                                    "ERROR")))),
           opt_outs_excluded = enrollment - parent_opt_out,
           group_by = "All Students",
           group_by_value = "All Students") %>%
    modify_at("student_count", as.integer) %>%
    modify_at("group_count", as.integer) %>%
    filter(grade != "Total" & !str_detect(school_name, "Choice Program")) %>%
    select(-c(enrollment))
  }
  
  
  choice_forward <- bind_rows(choice_forward, choice1_forward)
  
}

choice_program <- choice_forward %>%
  select(school_year:WPCP, dpi_true_id) %>%
  unique()

unique_schools <- unique_schools %>%
  left_join(., choice_program, by = c("last_year_open" = "school_year", "dpi_true_id")) %>%
  mutate(MPCP = ifelse(!is.na(MPCP), 1, 0),
         RPCP = ifelse(!is.na(RPCP), 1, 0),
         WPCP = ifelse(!is.na(WPCP), 1, 0)) 

choice_forward <- choice_forward %>%
  filter(test_group == "Forward")



files <- list.files(path = "./imports/wsas/public")

# Set to NULL because using !exists() doesn't work in for loop
public_forward <- NULL

for(file in files) {
  
  filename <- paste("imports/wsas/public", file, sep = "/")
  
  if(is.null("public_forward")) {
    raw_public <- read_csv(filename, col_types = list("c",
                                                      "f",
                                                      "i",
                                                      "c",
                                                      "i",
                                                      "i",
                                                      "f",
                                                      "l",
                                                      "c",
                                                      "c",
                                                      "f",
                                                      "f",
                                                      "f",
                                                      "i",
                                                      "f",
                                                      "f",
                                                      "f",
                                                      "i",
                                                      "c",
                                                      "i",
                                                      "c"))
    
    colnames(raw_public) <- str_to_lower(colnames(raw_public))
    
    public_forward <- raw_public %>%
      filter(!str_detect(school_name, pattern = "\\[")) %>%
      mutate(district_code = str_pad(district_code, 4, side = "left", pad = 0),
             school_code = str_pad(school_code, 4, side = "left", pad = 0),
             dpi_true_id = paste(district_code, school_code, sep = "_")) %>%
      select(school_year,
             dpi_true_id,
             test_subject,
             "grade" = grade_level,
             test_result,
             test_group,
             group_by,
             group_by_value,
             student_count,
             group_count)
  }
  
  else {raw_public1 <- read_csv(filename, col_types = list("c",
                                                           "f",
                                                           "i",
                                                           "c",
                                                           "i",
                                                           "i",
                                                           "f",
                                                           "l",
                                                           "c",
                                                           "c",
                                                           "f",
                                                           "f",
                                                           "f",
                                                           "i",
                                                           "f",
                                                           "f",
                                                           "f",
                                                           "i",
                                                           "c",
                                                           "i",
                                                           "c"))
  
  colnames(raw_public1) <- str_to_lower(colnames(raw_public1))
  
  public1_forward <- raw_public1 %>%
    filter(!str_detect(school_name, pattern = "\\[")) %>%
    mutate(district_code = str_pad(district_code, 4, side = "left", pad = 0),
           school_code = str_pad(school_code, 4, side = "left", pad = 0),
           dpi_true_id = paste(district_code, school_code, sep = "_")) %>%
    select(school_year,
           dpi_true_id,
           test_subject,
           "grade" = grade_level,
           test_result,
           test_group,
           group_by,
           group_by_value,
           student_count,
           group_count)
  }
  
  
  public_forward <- bind_rows(public_forward, public1_forward)
}

forward_exam <- full_join(public_forward, choice_forward) %>%
  select(-c(MPCP, RPCP, WPCP, school_name))

# Choice Counts ====

choice_names <- read_csv("imports/choice_names.csv")

files <- list.files(path = "./imports/choice_counts")

choice_counts <- NULL

for(file in files) {
  
  filename <- paste("imports/choice_counts", file, sep = "/")
  
  cc <- readxl::read_xls(filename, skip = 4) %>%
    filter(!is.na(`...1`)) %>%
    select("school_name" = `School Name`,
           "MPCP_count" = `MPCP Student HC`,
           "RPCP_count" = `RPCP Student HC`,
           "WPCP_count" = `WPCP \nStudent HC`,
           "SNSP_count" = `SNSP Student HC`) %>%
    left_join(., choice_names, by = "school_name") %>%
    mutate(school_year = substr(file, 1, 7))
  
  choice_counts <- bind_rows(choice_counts, cc)
}






rc_renamed <- rc_renamed %>%
  select(-c(school_name,
            district_name,
            locale_description,
            city))



school_db <- dbConnect(RSQLite::SQLite(), "school_db.sqlite")

dbWriteTable(school_db, "schools", unique_schools, overwrite = TRUE)

dbWriteTable(school_db, "graduation", public_graduation, overwrite = TRUE)

dbWriteTable(school_db, "enrollment", only_enrollment, overwrite = TRUE)

dbWriteTable(school_db, "report_cards", rc_renamed, overwrite = TRUE)

dbWriteTable(school_db, "forward_exam", forward_exam, overwrite = TRUE)

dbWriteTable(school_db, "choice_counts", choice_counts, overwrite = TRUE)

dbWriteTable(school_db, "open_enrollment", open_enrollment, overwrite = TRUE)

dbWriteTable(school_db, "chapter_220", chapter_220, overwrite = TRUE)

tables <- dbListTables(school_db)

for(i in 1:length(tables)) {
  saveRDS(dbReadTable(school_db, tables[i]), paste("imports/",
                                                   tables[i], ".rds", sep = ""))
}

schools <- readRDS("imports/schools.rds")

enrollment <- readRDS("imports/enrollment.rds")

report_cards <- readRDS("imports/report_cards.rds")
attr(report_cards, "source") <- "School Report Card Data Download File: https://apps2.dpi.wi.gov/reportcards/"
attr(report_cards, "data_dictionary") <- read_csv("rc_data_dict.csv")

forward_exam <- readRDS("imports/forward_exam.rds")

graduation <- readRDS("imports/graduation.rds")

choice_counts <- readRDS("imports/choice_counts.rds")

open_enrollment <- readRDS("imports/open_enrollment.rds")

chapter_220 <- readRDS("imports/chapter_220.rds")

save(list = c("schools", "enrollment", "report_cards", "forward_exam", "graduation", "choice_counts", "open_enrollment", "chapter_220"),
     file = "C:/Users/Spencer/repor/wisconsink12/data/school_data.RData")

dbDisconnect(school_db)

rm(list = ls())