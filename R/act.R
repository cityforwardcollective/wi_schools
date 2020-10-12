# ACT Scores
# Sources: 
# Public -- https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=ACT11
# Private -- https://dpi.wi.gov/assessment/parental-choice-program/data

library(tidyverse)
library(readxl)

make_act <- function() {
  ## Private Schools
  
  files <- list.files(path = "./imports/wsas/private")
  
  # Set to NULL because using !exists() doesn't work in for loop
  choice_act <- choice_act1 <- NULL
  
  for (file in files) {
    
    filename <- paste("imports/wsas/private", file, sep = "/")
    
    if (length(choice_act) == 0) {
      
      if (str_detect(filename, "2015")) {
        raw_choice <- read_xlsx(filename, "ACT Score Data", col_names = TRUE)
      } else {
        raw_choice <- read_xlsx(filename, "ACT Score Data", skip = 1, col_names = TRUE)
        
      }
      
      raw_choice <- raw_choice %>% 
        filter(!str_detect(`School Name and Number`, "Choice Program")) %>%
        mutate(dpi_true_id = ifelse(str_detect(`School Name and Number`, "\\("), 
                                    str_pad(str_replace_all(str_extract(`School Name and Number`, "\\(.*\\)$"), "\\(|\\)", ""), width = 4, side = "left", pad = "0"),
                                    str_pagd(str_sub(`School Name and Number`, start = -4, end = -1), width = 4, side = "left", pad = "0")), # this handles 2015-16 data where no () are present
               dpi_true_id = ifelse(is.na(dpi_true_id), dpi_true_id, paste("0000", dpi_true_id, sep = "_")),
               school_name = ifelse(!str_detect(`School Name and Number`, "-|\\("), `School Name and Number`,
                                    str_replace(str_extract(`School Name and Number`, ".*-|.*\\("), "-|\\(", "")))
      
      colnames(raw_choice) <- str_to_lower(str_replace_all(colnames(raw_choice), pattern = " ", replacement = "_")) 
      
      scores <- raw_choice %>%
        select(dpi_true_id, contains("average")) %>%
        pivot_longer(cols = contains("average"), names_to = "subject", values_to = "score") %>%
        mutate(subject = str_replace_all(subject, "average|_|score", ""))
      
      counts <- raw_choice %>%
        select(dpi_true_id, contains("count")) %>%
        pivot_longer(cols = contains("count"), names_to = "subject", values_to = "count") %>%
        mutate(subject = str_replace_all(subject, "count|_|of", ""))
      
      choice_act <- left_join(scores, counts, by = c("dpi_true_id", "subject")) %>%
        mutate(school_year = ifelse(str_detect(filename, "\\d-\\d"), str_extract(filename, "\\d{4}-\\d{2}"),
                                    paste(as.numeric(str_extract(filename, "\\d{4}")) - 1, 
                                          str_sub(str_extract(filename, "\\d{4}"), 3, 4), sep = "-")))
      
      
    } else {
      
      if (str_detect(filename, "2015")) {
        raw_choice1 <- read_xlsx(filename, "ACT Score Data", col_names = TRUE)
      } else {
        raw_choice1 <- read_xlsx(filename, "ACT Score Data", skip = 1, col_names = TRUE)
        
      }
      
      raw_choice1 <- raw_choice1 %>% 
        filter(!str_detect(`School Name and Number`, "Choice Program")) %>%
        mutate(dpi_true_id = ifelse(str_detect(`School Name and Number`, "\\("), 
                                    str_pad(str_replace_all(str_extract(`School Name and Number`, "\\(.*\\)$"), "\\(|\\)", ""), width = 4, side = "left", pad = "0"),
                                    str_pad(str_sub(`School Name and Number`, start = -4, end = -1), width = 4, side = "left", pad = "0")), # this handles 2015-16 data where no () are present
               dpi_true_id = ifelse(is.na(dpi_true_id), dpi_true_id, paste("0000", dpi_true_id, sep = "_")),
               school_name = ifelse(!str_detect(`School Name and Number`, "-|\\("), `School Name and Number`,
                                    str_replace(str_extract(`School Name and Number`, ".*-|.*\\("), "-|\\(", "")))
      
      
      colnames(raw_choice1) <- str_to_lower(str_replace_all(colnames(raw_choice1), pattern = " ", replacement = "_")) 
      
      scores <- raw_choice1 %>%
        select(dpi_true_id, contains("average")) %>%
        pivot_longer(cols = contains("average"), names_to = "subject", values_to = "score") %>%
        mutate(subject = str_replace_all(subject, "average|_|score", ""))
      
      counts <- raw_choice1 %>%
        select(dpi_true_id, contains("count")) %>%
        pivot_longer(cols = contains("count"), names_to = "subject", values_to = "count") %>%
        mutate(subject = str_replace_all(subject, "count|_|of", ""))
      
      choice_act1 <- left_join(scores, counts, by = c("dpi_true_id", "subject")) %>%
        mutate(school_year = ifelse(str_detect(filename, "\\d-\\d"), str_extract(filename, "\\d{4}-\\d{2}"),
                                    paste(as.numeric(str_extract(filename, "\\d{4}")) - 1, 
                                          str_sub(str_extract(filename, "\\d{4}"), 3, 4), sep = "-")))
    }
    
    
    choice_act <- bind_rows(choice_act, choice_act1)
    
    
  }
  
  choice_act <- choice_act %>%
    modify_at(c("score", "count"), as.numeric) %>%
    modify_at("score", round, 1) %>%
    rename("test_subject" = subject,
           "student_count" = count,
           "average_score" = score) %>%
    mutate(group_by = "All Students",
           group_by_value = "All Students",
           test_group = "ACT",
           test_subject = ifelse(test_subject == "composite", "Composite",
                                 ifelse(test_subject == "ela", "ELA",
                                        ifelse(test_subject == "math", "Mathematics",
                                               ifelse(test_subject == "science", "Science", test_subject)))),
           test_result = ifelse(average_score >= 28, "Advanced",
                                ifelse(test_subject == "ELA" & average_score >= 20, "Proficient",
                                       ifelse(test_subject == "ELA" & average_score >= 15, "Basic",
                                              ifelse(test_subject == "Mathematics" & average_score >= 22, "Proficient",
                                                     ifelse(test_subject == "Mathematics" & average_score >= 17, "Basic",
                                                            ifelse(test_subject == "Science" & average_score >= 23, "Proficient",
                                                                   ifelse(test_subject == "Science" & average_score >= 18, "Basic",
                                                                          ifelse(test_subject == "Composite", "Not Benchmarked", "Below Basic")))))))))
  
  # Public ACT ====
  
  files <- list.files(path = "./imports/wsas/public_act")
  
  # Set to NULL because using !exists() doesn't work in for loop
  public_act <- public_act1 <-  NULL
  
  for (file in files) {
    
    filename <- paste("imports/wsas/public_act", file, sep = "/")
    
    if (is.null(public_act)) {
      raw_public_act <- read_csv(filename, col_types = cols(SCHOOL_CODE = col_integer()))
      
      colnames(raw_public_act) <- str_to_lower(colnames(raw_public_act))
      
      public_act <- raw_public_act %>%
        filter(!str_detect(school_name, pattern = "\\[")) %>%
        mutate(district_code = str_pad(district_code, 4, side = "left", pad = 0),
               school_code = str_pad(school_code, 4, side = "left", pad = 0),
               dpi_true_id = paste(district_code, school_code, sep = "_")) %>%
        select(school_year,
               dpi_true_id,
               test_subject,
               test_result,
               test_group,
               group_by,
               group_by_value,
               average_score,
               student_count) %>%
        modify_at(c("group_count", "student_count"), as.numeric)
      
    } else {
      raw_public_act <- read_csv(filename, col_types = cols(SCHOOL_CODE = col_integer()))
      
      colnames(raw_public_act) <- str_to_lower(colnames(raw_public_act))
      
      public_act1 <- raw_public_act %>%
        filter(!str_detect(school_name, pattern = "\\[")) %>%
        mutate(district_code = str_pad(district_code, 4, side = "left", pad = 0),
               school_code = str_pad(school_code, 4, side = "left", pad = 0),
               dpi_true_id = paste(district_code, school_code, sep = "_")) %>%
        select(school_year,
               dpi_true_id,
               test_subject,
               test_result,
               test_group,
               group_by,
               group_by_value,
               average_score,
               student_count) %>%
        modify_at(c("group_count", "student_count"), as.numeric)
    }
    
    
    public_act <- bind_rows(public_act, public_act1)
  }
  
  public_act$average_score <- as.numeric(public_act$average_score)
  act <- bind_rows(public_act, choice_act)

  return(act)  
}

act <- make_act()