# Choice Counts
# Source: https://dpi.wi.gov/sms/choice-programs/data
#
# NOTE: Uses 3rd Friday Head Count

choice_names <- read_csv("imports/choice_names.csv")

files <- list.files(path = "imports/choice_counts")

choice_counts <- NULL

for (file in files) {
  
  filename <- paste("imports/choice_counts", file, sep = "/")
  
  cc <- readxl::read_xls(filename, skip = 4) %>%
    filter(!is.na(`...1`)) %>%
    select("school_name" = `School Name`,
           "MPCP_count" = `MPCP Student HC`,
           "RPCP_count" = `RPCP Student HC`,
           "WPCP_count" = `WPCP \nStudent HC`,
           "SNSP_count" = `SNSP Student HC`) %>%
    left_join(., choice_names, by = "school_name") %>%
    mutate(school_year = substr(file, 1, 7)) %>%
    select(-c(MPCP, RPCP, WPCP, SNSP))
  
  choice_counts <- bind_rows(choice_counts, cc)
}