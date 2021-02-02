library(DBI)
library(RSQLite)

{
  # Choice Counts
  
  source("R/choice_counts.R")
  
  # Open Enrollment
  
  source("R/open_enrollment.R")
  
  # Enrollment
  
  source("R/enrollment.R")
  
  only_enrollment <- all_enrollment %>%
    mutate(student_count = as.numeric(student_count)) %>%
    select(school_year, dpi_true_id, group_by, group_by_value, student_count)
  
  grade_levels <- all_enrollment %>%
    filter(group_by == "Grade Level") %>%
    modify_at("group_by_value", factor, levels = c("PK",
                                                   "K3",
                                                   "K4",
                                                   "KG",
                                                   as.character(seq(from = 1, to = 12, by = 1))), ordered = TRUE) %>%
    filter(student_count > 0 & !is.na(group_by_value)) %>%
    group_by(school_year, dpi_true_id, school_name) %>%
    summarise(lowest_enrolled_grade = min(group_by_value),
              highest_enrolled_grade = max(group_by_value))
  
  # Attendance
  
  source("R/attendance.R")
  
  # Discipline
  
  source("R/discipline.R")
  
  # Retention (Grade Promotion)
  
  source("R/retention.R")
  
  # Report Cards
  
  source("R/report_cards.R")
  
  report_cards <- rc_renamed %>%
    select(-c(school_name,
              district_name,
              locale_description,
              city))
  
  # WSAS
  
  source("R/wsas.R")
  
  # ACT
  
  source("R/act.R")
  
  # High School Completion
  
  source("R/high_school_completion.R")
  
  # Chapter 220
  
  school_year <- c("2015-16", 
                   "2016-17", 
                   "2017-18", 
                   "2018-19", 
                   "2019-20")
  
  total_enrollment <- c(1266, 
                        1038, 
                        842, 
                        672, 
                        536)
  
  chapter_220 <- tibble(school_year, total_enrollment, 
                        accurate_agency_type = "Chapter 220",
                        broad_agency_type = "Open Enrollment/Chapter 220")
  
  # Other Enrollment
  
  mke_oe <- open_enrollment %>%
    filter(district_code == 3619) %>%
    select(school_year, pupil_transfers_out) %>%
    rename("total_enrollment" = pupil_transfers_out) %>%
    mutate(accurate_agency_type = "Open Enrollment",
           broad_agency_type = "Open Enrollment/Chapter 220")
  
  mobile_students <- rc_renamed %>%
    filter(district_name == "Milwaukee") %>%
    group_by(school_year, district_enrollment) %>%
    summarise(total_enrollment = sum(school_enrollment, na.rm = TRUE)) %>%
    mutate(total_enrollment = district_enrollment - total_enrollment,
           accurate_agency_type = "Mobile Students",
           broad_agency_type = "District Operated") %>%
    select(-district_enrollment)
  
  
  other_enrollment <- bind_rows(mobile_students, mke_oe, chapter_220)
  
  # Schools
  
  ## Instrumentality and Partnership
  ## NOTE: MATC Emerging Scholars is listed as a Parternship School,
  ## but the students are tracked in "home school" SIS, so it doesn't
  ## have a school code/appear in reports.
  
  other_schools <- read_csv("imports/inst_partner_schools.csv") %>%
    filter(!is.na(dpi_true_id)) %>%
    select(-school_name)
  
  
  ## All fields sourced from Enrollment reports
  
  schools_e <- all_enrollment %>%
    select(school_year:district_name,
           enrollment_charter_indicator,
           enrollment_choice_identifier,
           county) %>%
    mutate(charter_indicator = ifelse(enrollment_charter_indicator == "Yes", 1, 0),
           choice_indicator = ifelse(enrollment_choice_identifier == "CHC", 1, 0)) %>%
    select(-c(enrollment_charter_indicator, enrollment_choice_identifier)) %>%
    unique()
  
  with_others <- schools_e %>%
    semi_join(., other_schools %>% select(-accurate_agency_type)) %>%
    left_join(., other_schools)
  
  not_others <- schools_e %>%
    
    # anti_join removes inst and partnership schools
    
    anti_join(., other_schools %>% select(-accurate_agency_type)) %>%
    mutate(choice_indicator = ifelse(is.na(choice_indicator), 0, choice_indicator),
           accurate_agency_type = ifelse(agency_type == "Non District Charter Schools", "2r/2x Charter",
                                         ifelse(agency_type == "Private school", "Private",
                                                ifelse(agency_type == "Public school" & charter_indicator == 1, "Non-Instrumentality Charter",
                                                       "Traditional Public"))))
  schools_bat <- bind_rows(with_others, not_others) %>%
    mutate(broad_agency_type = ifelse(accurate_agency_type %in% c("2r/2x Charter", "Non-Instrumentality Charter", "Partnership"), "Independently Operated",
                                    ifelse(accurate_agency_type == "Private", "Private",
                                           "District Operated")))
  
  
  
  ## Add individual choice program indicators
  
  schools_c <- schools_bat %>%
    left_join(., choice_counts %>% select(-school_name)) %>%
    mutate(MPCP_indicator = ifelse(!is.na(MPCP_count), 1, 0),
           RPCP_indicator = ifelse(!is.na(RPCP_count), 1, 0),
           WPCP_indicator = ifelse(!is.na(WPCP_count), 1, 0),
           SNSP_indicator = ifelse(!is.na(SNSP_count), 1, 0),
           MPCP_has_students = ifelse(MPCP_indicator == 1 & MPCP_count > 0, 1, 0),
           RPCP_has_students = ifelse(RPCP_indicator == 1 & RPCP_count > 0, 1, 0),
           WPCP_has_students = ifelse(WPCP_indicator == 1 & WPCP_count > 0, 1, 0),
           SNSP_has_students = ifelse(SNSP_indicator == 1 & SNSP_count > 0, 1, 0)) %>%
    select(-c(contains("_count")))
  
  ## Add report card variables
  ## locale_description and city
  
  schools_rc <- schools_c %>%
    left_join(., rc_renamed %>% select(dpi_true_id, school_year, locale_description, city))
  
  schools_1920 <- schools_rc %>%
    filter(school_year == "2019-20") %>%
    select(-c(city, locale_description))
  
  schools_1819 <- schools_rc %>%
    filter(school_year == "2018-19") %>%
    select(dpi_true_id, city, locale_description) %>%
    unique()
  
  guesses <- left_join(schools_1920, schools_1819)
  
  schools <- schools_rc %>%
    filter(school_year != "2019-20") %>%
    bind_rows(., guesses) %>%
    unique() %>%
    left_join(., choice_counts %>% select(school_year, dpi_true_id, MPCP_count, ALL_STUDENTS_count)) %>%
    mutate(MPCP_percent = MPCP_count / ALL_STUDENTS_count,
           MPCP_percent = replace_na(MPCP_percent, 0)) %>%
    # Exclude Milwaukee Private schools that don't have publicly funded students
    mutate(milwaukee_indicator = ifelse(district_name == "Milwaukee" & !(agency_type == "Private school" & MPCP_has_students == 0 & SNSP_has_students == 0), 1,
                                        ifelse(is.na(city), 0,
                                               ifelse(city == "Milwaukee" & locale_description == "City", 1, # Gets rid of suburbs
                                                      ifelse(MPCP_percent > 0.749, 1, 0))))) %>% # For MPCP outside of district
    select(-c(ALL_STUDENTS_count, MPCP_count))
  
  schools <- left_join(schools, grade_levels)
  
  nrow(schools_rc) == nrow(schools)
  
  # Build Database
  
  school_db <- dbConnect(RSQLite::SQLite(), "school_db.sqlite")
  
  dbWriteTable(school_db, "schools", schools, overwrite = TRUE)
  
  dbWriteTable(school_db, "graduation", public_graduation, overwrite = TRUE)
  
  dbWriteTable(school_db, "enrollment", only_enrollment, overwrite = TRUE)
  
  dbWriteTable(school_db, "attendance", attendance, overwrite = TRUE)
  
  dbWriteTable(school_db, "discipline", discipline, overwrite = TRUE)
  
  dbWriteTable(school_db, "retention", retention, overwrite = TRUE)
  
  dbWriteTable(school_db, "report_cards", report_cards, overwrite = TRUE)
  
  dbWriteTable(school_db, "forward_exam", forward_exam, overwrite = TRUE)
  
  dbWriteTable(school_db, "act", act, overwrite = TRUE)
  
  dbWriteTable(school_db, "choice_counts", choice_counts, overwrite = TRUE)
  
  dbWriteTable(school_db, "other_enrollment", other_enrollment, overwrite = TRUE)
  
  tables <- dbListTables(school_db)
  
  for (i in 1:length(tables)) {
    saveRDS(dbReadTable(school_db, tables[i]), paste("imports/",
                                                     tables[i], ".rds", sep = ""))
  }
  
  schools <- readRDS("imports/schools.rds")
  attr(schools, "data_dictionary") <- read_csv("data_dictionaries/schools/schools_data_dictionary.csv")
  
  enrollment <- readRDS("imports/enrollment.rds")
  
  attendance <- readRDS("imports/attendance.rds")
  
  discipline <- readRDS("imports/discipline.rds")
  
  retention <- readRDS("imports/retention.rds")
  
  report_cards <- readRDS("imports/report_cards.rds")
  attr(report_cards, "source") <- "School Report Card Data Download File: https://apps2.dpi.wi.gov/reportcards/"
  attr(report_cards, "data_dictionary") <- read_csv("data_dictionaries/report_cards/report_cards_data_dictionary.csv")
  
  forward_exam <- readRDS("imports/forward_exam.rds")
  
  graduation <- readRDS("imports/graduation.rds")
  
  choice_counts <- readRDS("imports/choice_counts.rds")
  
  other_enrollment <- readRDS("imports/other_enrollment.rds")
  
  act <- readRDS("imports/act.rds")
  
  save(list = c("schools", "enrollment", "attendance", "discipline", "retention",
                "report_cards", "forward_exam", "graduation", "choice_counts", "other_enrollment", "act"),
       file = "C:/Users/Spencer/repor/wisconsink12/data/school_data.RData")
  
  dbDisconnect(school_db)
  
  rm(list = ls())
}
