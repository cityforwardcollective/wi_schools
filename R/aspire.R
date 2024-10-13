# ACT Aspire publicScores
# Sources: 
# Public -- https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Aspire

library(tidyverse)
library(readxl)

make_aspire <- function() {
  
  # Public ACT ====
  
  files <- list.files(path = "./imports/wsas/public_aspire")
  
  # Set to NULL because using !exists() doesn't work in for loop
  public_act <- public_act1 <-  NULL
  
  for (file in files) {
    
    filename <- paste("imports/wsas/public_aspire", file, sep = "/")
    
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
               grade = grade_level,
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
               grade = grade_level,
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
  act <- public_act %>%
    mutate(test_group = ifelse(test_group == "ASPIRE", "Aspire", test_group),
           grade = as.character(grade)) %>%
    select(-average_score)

  return(act)  
}

aspire <- make_aspire()
