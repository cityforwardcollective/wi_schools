a_schools <- all_enrollment %>%
  modify_at("school_year", factor, ordered = TRUE) %>%
  select(school_year:district_name,
         enrollment_charter_indicator,
         enrollment_choice_identifier,
         enrollment_county = county) %>%
  unique()


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
                                                     ifelse(dpi_true_id %in% partnership, "Partnership",
                                                            ifelse(agency_type == "Public school" & charter_indicator == 1, "Non-Instrumentality Charter",
                                                                   "Traditional Public"))))),
         broad_agency_type = ifelse(accurate_agency_type %in% c("2r/2x Charter", "Non-Instrumentality Charter", "Partnership"), "Independently Operated",
                                    ifelse(accurate_agency_type == "Private", "Private",
                                           "District-Operated")))

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

choice_program <- choice_forward %>%
  select(school_year:WPCP, dpi_true_id) %>%
  unique() %>%
  filter(school_year == max(school_year)) %>%
  select(-school_year)

unique_schools <- unique_schools %>%
  left_join(., choice_program, by = c("dpi_true_id")) %>%
  mutate(MPCP = ifelse(is.na(MPCP), 0, 1),
         RPCP = ifelse(is.na(RPCP), 0, 1),
         WPCP = ifelse(is.na(WPCP), 0, 1)) 

## Test for mke choice schools filter error


test <- unique_schools %>%
  filter(dpi_true_id == "0000_1712" & MPCP == 1)

if (nrow(test) != 1) {
  
  stop("Milwaukee Choice Schools Error -- Cristo Rey not designated as MPCP. See unique_schools and choice_forward tables.")
  
}