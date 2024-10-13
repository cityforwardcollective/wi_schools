# Forward Exam
# Sources: 
# Public Forward/DLM -- https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Forward
# Private -- https://dpi.wi.gov/assessment/parental-choice-program/data
#
# NOTE: Opt Outs included in denominator for Private Schools to match Public methodology

library(tidyverse)
library(readxl)

make_forward_exam <- function() {
  # Private Schools
  
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
                     "No Test",
                     "Below Basic",
                     "Basic",
                     "Proficient",
                     "Advanced",
                     "dpi_true_id",
                     "school_name")
    
    x <- x %>%
      mutate_at(8:12, as.integer) %>%
      mutate_at(6:7, as.character) |> 
      mutate(group_count = `No Test` + `Below Basic` + Basic + Proficient + Advanced) %>%
      gather(key = "test_result", value = "student_count", `No Test`:Advanced)
  }
  
  for (file in files) {
    
    filename <- paste("imports/wsas/private", file, sep = "/")
    cat(crayon::cyan("Starting ", filename, "\n"))
    
    if (length(choice_forward) == 0) {
      raw_choice <- read_xlsx(filename, "Opt Out Included", skip = 1, col_names = TRUE)
      
      raw_choice <- raw_choice %>% 
        select(-contains("%")) %>%
        mutate(dpi_true_id = str_replace_all(str_extract(raw_choice$`School Name and Number`, "\\(.*\\)$"), "\\(|\\)", ""),
               dpi_true_id = ifelse(is.na(dpi_true_id), dpi_true_id, 
                                    paste("0000", 
                                          str_pad(dpi_true_id, width = 4, side = "left", pad = "0"), sep = "_")),
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
        mutate(
          # old nested if-else replaced with case_when
          # test_group = ifelse(grade %in% 3:8, "Forward",
          #                          ifelse(grade == 10 & test_subject == "social studies", "Forward",
          #                                 ifelse(grade %in% 9:10, "Aspire",
          #                                        ifelse(grade == 11, "ACT",
          #                                               "ERROR")))),
               test_group = case_when(
                 grade %in% 3:8 ~ "Forward",
                 grade == 10 & test_subject == "social studies" ~ "Forward",
                 grade %in% 9:10 & school_year < "2019-20" ~ "Aspire",
                 grade %in% 9:10 & school_year >= "2019-20" ~ "PreACT",
                 grade == 11 ~ "ACT",
                 TRUE ~ "ERROR"
               ),
               opt_outs_excluded = as.numeric(enrollment) - as.numeric(parent_opt_out),
               group_by = "All Students",
               group_by_value = "All Students") %>%
        modify_at("student_count", as.integer) %>%
        filter(grade != "Total" & !str_detect(school_name, "All Choice|Choice Program")) %>%
        select(-c(enrollment))
    } else {
      
      raw_choice1 <- read_xlsx(filename, "Opt Out Included", skip = 1, col_names = TRUE)
      
      raw_choice1 <- raw_choice1 %>% select(-contains("%")) %>%
        mutate(dpi_true_id = str_replace_all(str_extract(raw_choice1$`School Name and Number`, "\\(.*\\)$"), "\\(|\\)", ""),
               dpi_true_id = ifelse(is.na(dpi_true_id), dpi_true_id, 
                                                  paste("0000", 
                                                        str_pad(dpi_true_id, width = 4, side = "left", pad = "0"), sep = "_")),
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
        mutate(
          # test_group = ifelse(grade %in% 3:8, "Forward",
          #                          ifelse(grade == 10 & test_subject == "social studies", "Forward",
          #                                 ifelse(grade %in% 9:10, "Aspire",
          #                                        ifelse(grade == 11, "ACT",
          #                                               "ERROR")))),
          test_group = case_when(
            grade %in% 3:8 ~ "Forward",
            grade == 10 & test_subject == "social studies" ~ "Forward",
            grade %in% 9:10 & school_year < "2019-20" ~ "Aspire",
            grade %in% 9:10 & school_year >= "2019-20" ~ "PreACT",
            grade == 11 ~ "ACT",
            TRUE ~ "ERROR"
          ),
               opt_outs_excluded = as.numeric(enrollment) - as.numeric(parent_opt_out),
               group_by = "All Students",
               group_by_value = "All Students") %>%
        modify_at("student_count", as.integer) %>%
        modify_at("group_count", as.integer) %>%
        filter(grade != "Total" & !str_detect(school_name, "All Choice|Choice Program")) %>%
        select(-c(enrollment))
    }
    
    
    choice_forward <- bind_rows(choice_forward, choice1_forward)
    
  }
  
  choice_forward <- choice_forward 
  
  
  # Public Schools ====
  
  files <- list.files(path = "./imports/wsas/public")
  
  # Set to NULL because using !exists() doesn't work in for loop
  public_forward <- NULL
  
  for (file in files) {
    
    filename <- paste("imports/wsas/public", file, sep = "/")
    cat(crayon::cyan("Starting ", filename, "\n"))
    
    
    if (is.null("public_forward")) {
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
  
  return(forward_exam)
  
}

forward_exam <- make_forward_exam()
