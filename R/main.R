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

# WSAS

source("R/wsas.R")

# ACT

source("R/act.R")

# High School Completion

source("R/high_school_completion")

# Chapter 220

school_year <- c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20")

total_enrollment <- c(1266 , 1038, 842, 672, 536)

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

## remove school year filter, add "last year open" field

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

## NOTE: locale description is "NA" in source files
## for private schools

unique_schools <- left_join(unique_schools, rc1, by = "dpi_true_id")