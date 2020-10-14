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
           broad_agency_type = "District-Operated") %>%
    select(-district_enrollment)
  
  
  other_enrollment <- bind_rows(mobile_students, mke_oe, chapter_220)
  
  # Schools
  
  instrumentality <- c("3619_0162",
                       "3619_0413",
                       "3619_0334",
                       "3619_0165",
                       "3619_0398",
                       "3619_0454")
  
  partnership <- c("3619_1063", # Assata
                   "3619_0432", # Banner Prep
                   "3619_0410", # Grandview
                   "3619_0296", # Lad Lake
                   "3619_1072", # NOVA
                   "3619_1074", # Shalom
                   "3619_1086") # Southeastern
  
  ## All fields sourced from Enrollment reports
  
  schools_e <- all_enrollment %>%
    select(school_year:district_name,
           enrollment_charter_indicator,
           enrollment_choice_identifier,
           county) %>%
    mutate(charter_indicator = ifelse(enrollment_charter_indicator == "Yes", 1, 0),
           choice_indicator = ifelse(enrollment_choice_identifier == "CHC", 1, 0)) %>%
    select(-c(enrollment_charter_indicator, enrollment_choice_identifier)) %>%
    unique() %>%
    mutate(choice_indicator = ifelse(is.na(choice_indicator), 0, choice_indicator),
           accurate_agency_type = ifelse(agency_type == "Non District Charter Schools", "2r/2x Charter",
                                         ifelse(agency_type == "Private school", "Private",
                                                ifelse(dpi_true_id %in% instrumentality, "Instrumentality Charter",
                                                       ifelse(dpi_true_id %in% partnership, "Partnership",
                                                              ifelse(agency_type == "Public school" & charter_indicator == 1, "Non-Instrumentality Charter",
                                                                     "Traditional Public"))))),
           broad_agency_type = ifelse(accurate_agency_type %in% c("2r/2x Charter", "Non-Instrumentality Charter", "Partnership"), "Independently Operated",
                                      ifelse(accurate_agency_type == "Private", "Private",
                                             "District Operated")))
  
  ## Add individual choice program indicators
  
  schools_c <- schools_e %>%
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
    mutate(milwaukee_indicator = ifelse(district_name == "Milwaukee", 1,  # Public schools or Private within district limits
                                        ifelse(is.na(city), 0,
                                               ifelse(city == "Milwaukee" & locale_description == "City", 1, # Gets rid of suburbs
                                                      ifelse(MPCP_percent > 0.49, 1, 0))))) %>% # For MPCP outside of district
    select(-c(ALL_STUDENTS_count, MPCP_count))
  
  nrow(schools_rc) == nrow(schools)
  
  # Build Database
  
  school_db <- dbConnect(RSQLite::SQLite(), "school_db.sqlite")
  
  dbWriteTable(school_db, "schools", schools, overwrite = TRUE)
  
  dbWriteTable(school_db, "graduation", public_graduation, overwrite = TRUE)
  
  dbWriteTable(school_db, "enrollment", only_enrollment, overwrite = TRUE)
  
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
  
  report_cards <- readRDS("imports/report_cards.rds")
  attr(report_cards, "source") <- "School Report Card Data Download File: https://apps2.dpi.wi.gov/reportcards/"
  attr(report_cards, "data_dictionary") <- read_csv("data_dictionaries/report_cards/report_cards_data_dictionary.csv")
  
  forward_exam <- readRDS("imports/forward_exam.rds")
  
  graduation <- readRDS("imports/graduation.rds")
  
  choice_counts <- readRDS("imports/choice_counts.rds")
  
  other_enrollment <- readRDS("imports/other_enrollment.rds")
  
  act <- readRDS("imports/act.rds")
  
  save(list = c("schools", "enrollment", "report_cards", "forward_exam", "graduation", "choice_counts", "other_enrollment", "act"),
       file = "C:/Users/Spencer/repor/wisconsink12/data/school_data.RData")
  
  dbDisconnect(school_db)
  
  rm(list = ls())
}
