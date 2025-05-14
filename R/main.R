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
  
  # Public Aspire
  
  source("R/aspire.R")
  
  forward_exam <- bind_rows(forward_exam, aspire) 
  
  # ACT
  
  source("R/act.R")
  
  # High School Completion
  
  source("R/high_school_completion.R")
  
  # Chapter 220
  
  school_year <- c("2015-16", 
                   "2016-17", 
                   "2017-18", 
                   "2018-19", 
                   "2019-20",
                   "2020-21",
                   "2021-22",
                   "2022-23",
                   "2023-24")
  
  total_enrollment <- c(1266, 
                        1038, 
                        842, 
                        672, 
                        536,
                        429,
                        312,
                        217,
                        155)
  
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
  ## NOTE: MATC Emerging Scholars is listed as a Partnernship School,
  ## but the students are tracked in "home school" SIS, so it doesn't
  ## have a school code/appear in reports.
  
  o_schools <- read_csv("imports/inst_partner_schools.csv") %>%
    filter(!is.na(dpi_true_id)) %>%
    select(-school_name)
  
  others <- read_csv("imports/INST_NONI_2012_to_Current.csv", 
                     name_repair = janitor::make_clean_names) |> 
    mutate(district_code = str_pad(district_code, width = 4,
                                   side = "left", pad = "0"),
           school_code = str_pad(school_code, width = 4,
                                 side = "left", pad = "0"))
  inst <- others |> 
    mutate(dpi_true_id = glue::glue("{district_code}_{school_code}"),
           school_year = glue::glue("{year - 1}-{str_extract(year, '[:digit:]{2}$')}")) |> 
    transmute(school_year,
           dpi_true_id,
           accurate_agency_type = ifelse(instrumentality_status == "instrumentality",
                                         "Instrumentality Charter", 
                                         instrumentality_status)) |> 
    filter(accurate_agency_type == "Instrumentality Charter")
  
  other_schools <- o_schools |> 
    filter(accurate_agency_type != "Instrumentality Charter") |> 
    bind_rows(inst)
  
  other_schools |> 
    filter(school_year > "2020-21") |> 
    group_by(accurate_agency_type, school_year) |> 
    count()
  
  
  ## All fields sourced from Enrollment reports
  
  schools_e <- all_enrollment %>%
    select(school_year:cesa,
           enrollment_charter_indicator,
           enrollment_choice_identifier,
           county) %>%
    mutate(charter_indicator = ifelse(enrollment_charter_indicator == "Yes", 1, 0),
           choice_indicator = ifelse(enrollment_choice_identifier %in% c("CHC", "Yes"), 1, 0)) %>%
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
    left_join(., rc_renamed %>% 
                select(dpi_true_id, school_year, locale_description, city))
  
  rc_years <- report_cards %>%
    .[["school_year"]] %>%
    unique()
  
  schools_no_rc <- schools_rc %>%
    filter(!school_year %in% rc_years) %>%
    select(-c(city, locale_description))
  
  schools_max_rc <- schools_rc %>%
    filter(school_year == max(rc_years)) %>%
    select(dpi_true_id, city, locale_description) %>%
    unique()
  
  guesses <- left_join(schools_no_rc, schools_max_rc)
  
  #####
  
  schools <- schools_rc %>%
    filter(school_year %in% rc_years) %>%
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
    # below handles Notre Dame's campus that is outside Milwaukee;
    # choice counts aren't separated for each campus, so normal method 
    # doesn't capture it
    mutate(milwaukee_indicator = ifelse(dpi_true_id == "0000_1931", 1, milwaukee_indicator)) |> 
    select(-c(ALL_STUDENTS_count, MPCP_count))
  
  max_sy <- schools |> 
    filter(school_year == max(school_year) & 
             !accurate_agency_type %in% c("Private", "Traditional Public"))
  
  year <- unique(max_sy$school_year) |> 
    str_extract(pattern = "\\d{4}") |> 
    as.numeric()
  
  ly_mke <- schools |> 
    filter(school_year ==  glue::glue("{year - 1}-{year-2000}") &
             milwaukee_indicator == 1) |> 
    select(school_name, agency_type, county, milwaukee_indicator,
           city, locale_description)
  
  with_last <- left_join(max_sy |> 
              select(-c(milwaukee_indicator, city, locale_description)),
            ly_mke)
  
  schools <- anti_join(schools,
                       with_last |> 
                         select(school_year, dpi_true_id)) |> 
    bind_rows(with_last) |> 
    left_join(grade_levels %>% select(-school_name)) %>%
    arrange(school_name, school_year)
  
  nrow(schools_rc) == nrow(schools)
  
  # geocoded schools
  
  geo <- readRDS("imports/wi_schools_geocoded_2023-24.rda")
  
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
  
  dbWriteTable(school_db, "geocodes", geo, overwrite = TRUE)
  
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
  
  geocodes <- readRDS("imports/geocodes.rds")
  
  act <- readRDS("imports/act.rds")
  
  if (Sys.info()['sysname'] == "Darwin") {
    f <- "../wisconsink12/data/school_data.RData"
  } else {
    f <- "C:/Users/Spencer/repor/wisconsink12/data/school_data.RData"
  }
  
  save(list = c("schools", "enrollment", "attendance", "discipline", "retention",
                "report_cards", "forward_exam", "graduation", "choice_counts", 
                "other_enrollment", "act", "geocodes"),
       file = f)
  
  dbDisconnect(school_db)
  
  rm(list = ls())
}
