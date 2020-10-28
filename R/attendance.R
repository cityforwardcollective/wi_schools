library(tidyverse)

make_attendance <- function() {
  
  files <- list.files("imports/attendance/public")
  
  attendance <- map_df(files, function(x) {
    
    filename <- paste("imports/attendance/public", x, sep = "/")
    
    read_csv(filename) %>%
      filter(!SCHOOL_NAME %in% c("[Districtwide]", "[Statewide]")) %>%
      modify_at(c("STUDENT_COUNT", "POSSIBLE_DAYS_OF_ATTENDANCE", "ACTUAL_DAYS_OF_ATTENDANCE", "ATTENDANCE_RATE"),
                as.numeric) %>%
      mutate(SCHOOL_CODE = str_pad(SCHOOL_CODE, 4, side = "left", pad = "0"),
             dpi_true_id = paste(DISTRICT_CODE, SCHOOL_CODE, sep = "_"),
             attendance_rate = ATTENDANCE_RATE / 100) %>%
      select(school_year = SCHOOL_YEAR, 
             dpi_true_id,
             group_by = GROUP_BY, 
             group_by_value = GROUP_BY_VALUE, 
             student_count = STUDENT_COUNT, 
             possible_attendance_days = POSSIBLE_DAYS_OF_ATTENDANCE,
             actual_days_of_attendance = ACTUAL_DAYS_OF_ATTENDANCE,
             attendance_rate)
  })
  
  return(attendance)
}


attendance <- make_attendance()

