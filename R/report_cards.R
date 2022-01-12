library(tidyverse)
library(readxl)

make_report_cards <- function() {
  # Create vector of files in the folder
  
  files <- list.files(path = "./imports/report_cards")
  
  # Initialize objects for use in for loop
  
  rc <- rc_renamed <- rc_other <- NULL
  
  
  for (file in files) {
    
    # Create full file paths outside of if-else statements
    
    fileNames <- paste("imports/report_cards", file, sep = "/")
    
    # First conditional used if the initial tibble
    # has not been populated with any data.
    # Otherwise would overwrite with each loop.
    
    if (is.null(rc_renamed)) {
      
      rc <- read_xlsx(fileNames, sheet = "Data")
      
      rc_renamed <- rc %>%
        
        # Using `mutate` renames columns and populates with extant data,
        # while allowing for `ifelse` where certain fields don't exist
        # in all original files.  `select` would only allow for renaming.
        
        mutate(school_year = `School Year`,
               district_code = `District Code`,
               school_code = `School Code`,
               district_name = `District Name`,
               school_name = `School Name`,
               overall_score = `Overall Accountability Score`,
               overall_rating = `Overall Accountability Rating`,
               lowest_grade = `Lowest Grade in the School`,
               highest_grade = `Highest Grade in the School`,
               grade_band = `Grade Band for Comparison Schools`,
               school_type = `School Type`,
               school_enrollment = `School Enrollment`,
               district_enrollment = `District Enrollment`,
               per_am_in = `Percent American Indian or Alaskan Native`,
               per_asian = `Percent Asian`,
               per_b_aa = `Percent Black or African American`,
               per_hisp_lat = `Percent Hispanic/Latino`,
               per_nh_opi = `Percent Native Hawaiian or Other Pacific Islander`,
               per_white = `Percent White`,
               per_tom = `Percent Two or More Races`,
               per_swd = `Percent Students with Disabilities`,
               per_ed = `Percent Economically Disadvantaged`,
               per_lep = if ("Percent Limited English Proficient" %in% colnames(rc)) {
                                `Percent Limited English Proficient`
                 } else {
                                  `Percent English Learners`
                                },
               per_choice = `Percent School Choice Program`,
               per_open = `Percent Open Enrollment`,
               sch_ach = if ("School Student Achievement Score" %in% colnames(rc)) {
                 `School Student Achievement Score`
               } else { `School Achievement Score` },
               sch_ela_ach = `School ELA Achievement Score`,
               sch_math_ach = `School Mathematics Achievement Score`,
               sch_growth = if ("School Student Growth Score" %in% colnames(rc)) {
                 `School Student Growth Score`
               } else { `School Growth Score` },
               sch_ela_growth = `School ELA Growth Score`,
               sch_math_growth = `School Mathematics Growth Score`,
               sch_cg = if ("School Closing Gaps Score" %in% colnames(rc)) {
                 `School Closing Gaps Score`
               } else { NA },
               sch_ela_cg = if ("School Closing Gaps Score" %in% colnames(rc)) {
                 `School ELA Gap Score`
               } else { NA },
               sch_math_cg = if ("SChool Closing Gaps Score" %in% colnames(rc)) {
                 `School Mathematics Gap Score`
               } else { NA },
               
               # These fields don't exist in all files, so will throw an error
               # without using conditional.
               
               sch_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Outcomes Score`
               } else { NA },
               sch_ach_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Achievement Score`
               } else { NA },
               sch_growth_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Growth Score`
               } else { NA },
               sch_absenteeism_tgo =if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Chronic Absenteeism Score`
               } else { NA },
               sch_graduation_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Graduation Score`
               } else { NA },
               sch_attendance_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Attendance Score`
               } else { NA },
               sch_4y_grad_gap = if("School 4 Year Graduation Gap Score" %in% colnames(rc)) {
                 `School 4 Year Graduation Gap Score`} else {NA},
               sch_6y_grad_gap = if("School 6 Year Graduation Gap Score" %in% colnames(rc)) {
                 `School 6 Year Graduation Gap Score`} else {NA},
               sch_grad_gap = if ("School Graduation Gap Score" %in% colnames(rc)) {
                 `School Graduation Gap Score`
               } else { NA },
               sch_ot = if ("School On-Track and Postsecondary Readiness Score" %in% colnames(rc)) {
                 `School On-Track and Postsecondary Readiness Score`
               } else { `School On-Track to Graduation Score` },
               sch_grad_rate = `School Graduation Rate Score`,
               sch_att_rate = `School Attendance Rate Score`,
               sch_3rd_ela = if ("School Third-Grade ELA Achievement Score" %in% colnames(rc)) {
                 `School Third-Grade ELA Achievement Score`
               } else if ("School 3rd Grade ELA Achievement Score" %in% colnames(rc)) {
                 `School 3rd Grade ELA Achievement Score`
               } else { NA },
               sch_8th_math = if ("School Eighth-Grade Mathematics Achievement Score" %in% colnames(rc)) {
                 `School Eighth-Grade Mathematics Achievement Score`
               } else if ("School 8th Grade Mathematics Achievement Score" %in% colnames(rc)) {
                 `School 8th Grade Mathematics Achievement Score`
               } else { NA },
               ach_weight = if("Score weighting Achievement Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Achievement Priority Area`)} else {NA},
               growth_weight = if("Score weighting Growth Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Growth Priority Area`)} else {NA},
               cg_weight = if("Score weighting Closing Gaps Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Closing Gaps Priority Area`)} else {NA},
               tgo_weight = if("Score weighting Target Group Outcomes Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Target Group Outcomes Priority Area`)} else {NA},
               ot_weight = if("Score weighting Ontrack Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Ontrack Priority Area`)} else if (
                   "Score weighting On-Track Priority Area" %in% colnames(rc)
                   ) {
                     as.numeric(`Score weighting On-Track Priority Area`)}
                 else {NA},
               locale_description = `Locale description`,
               city = City,
               report_card_type = if("Report Card Type" %in% colnames(rc)) {
                 `Report Card Type`} else {NA},
               district_code = gsub("NA", 0000, district_code),
               district_code = str_pad(district_code, 4, side = "left", pad = 0),
               school_code = str_replace(school_code, "[:alpha:]", ""),
               school_code = str_pad(school_code, 4, side = "left", pad = 0),
               dpi_true_id = paste(district_code, school_code, sep = "_"),
               overall_score = round(as.numeric(overall_score), 1),
               school_enrollment = as.numeric(school_enrollment),
               district_enrollment = as.numeric(district_enrollment),
               cg_redacted = ifelse(str_detect(sch_cg, "\\*"), 1, 0),
               test_participation_ela = if ("Test Participation ELA 2021 All Students" %in% colnames(rc)) {
                 as.numeric(`Test Participation ELA 2021 All Students`)
               } else { NA },
               test_participation_math = if ("Test Participation Mathematics 2021 All Students" %in% colnames(rc)) {
                 as.numeric(`Test Participation Mathematics 2021 All Students`)
               } else { NA }) %>%
        mutate_at(vars("per_am_in":"per_open"), funs(round(as.numeric(.), 3))) %>%
        mutate_at(vars("sch_ach":"sch_8th_math"), funs(round(as.numeric(.), 1))) %>%
        
        # Since `mutate` was used above, columns were created instead of
        # being renamed.  Using select drops all original columns and leaves
        # just those we are interested in; also, using column names facilitates
        # functionality across files where column indices might differ.
        
        select(school_year,
               dpi_true_id,
               district_name,
               school_name,
               overall_score,
               overall_rating,
               lowest_grade,
               highest_grade,
               grade_band,
               school_type,
               school_enrollment,
               district_enrollment,
               per_am_in,
               per_asian,
               per_b_aa,
               per_hisp_lat,
               per_nh_opi,
               per_white,
               per_tom,
               per_swd,
               per_ed,
               per_lep,
               per_choice,
               per_open,
               sch_ach,
               sch_ela_ach,
               sch_math_ach,
               sch_growth,
               sch_ela_growth,
               sch_math_growth,
               sch_cg,
               sch_ela_cg,
               sch_math_cg,
               sch_tgo,
               sch_ach_tgo,
               sch_growth_tgo,
               sch_absenteeism_tgo,
               sch_graduation_tgo,
               sch_attendance_tgo,
               sch_4y_grad_gap,
               sch_6y_grad_gap,
               sch_grad_gap,
               sch_ot,
               sch_grad_rate,
               sch_att_rate,
               sch_3rd_ela,
               sch_8th_math,
               ach_weight,
               growth_weight,
               cg_weight,
               tgo_weight,
               ot_weight,
               locale_description,
               city,
               report_card_type,
               cg_redacted,
               test_participation_ela,
               test_participation_math)
    }
    
    # Second conditional used for following loops.
    # rc_other created then appended to rc_renamed.
    # rc_renamed grows with every loop.
    
    else {
      rc <- read_xlsx(fileNames, sheet = "Data")
      
      rc_other <- rc %>%
        mutate(school_year = `School Year`,
               district_code = `District Code`,
               school_code = `School Code`,
               district_name = `District Name`,
               school_name = `School Name`,
               overall_score = `Overall Accountability Score`,
               overall_rating = `Overall Accountability Rating`,
               lowest_grade = `Lowest Grade in the School`,
               highest_grade = `Highest Grade in the School`,
               grade_band = `Grade Band for Comparison Schools`,
               school_type = `School Type`,
               school_enrollment = `School Enrollment`,
               district_enrollment = `District Enrollment`,
               per_am_in = `Percent American Indian or Alaskan Native`,
               per_asian = `Percent Asian`,
               per_b_aa = `Percent Black or African American`,
               per_hisp_lat = `Percent Hispanic/Latino`,
               per_nh_opi = `Percent Native Hawaiian or Other Pacific Islander`,
               per_white = `Percent White`,
               per_tom = `Percent Two or More Races`,
               per_swd = `Percent Students with Disabilities`,
               per_ed = `Percent Economically Disadvantaged`,
               per_lep = if ("Percent Limited English Proficient" %in% colnames(rc)) {
                 `Percent Limited English Proficient`
               } else {
                 `Percent English Learners`
               },
               per_choice = `Percent School Choice Program`,
               per_open = `Percent Open Enrollment`,
               sch_ach = if ("School Student Achievement Score" %in% colnames(rc)) {
                 `School Student Achievement Score`
               } else { `School Achievement Score` },
               sch_ela_ach = `School ELA Achievement Score`,
               sch_math_ach = `School Mathematics Achievement Score`,
               sch_growth = if ("School Student Growth Score" %in% colnames(rc)) {
                 `School Student Growth Score`
               } else { `School Growth Score` },
               sch_ela_growth = `School ELA Growth Score`,
               sch_math_growth = `School Mathematics Growth Score`,
               sch_cg = if ("School Closing Gaps Score" %in% colnames(rc)) {
                 `School Closing Gaps Score`
               } else { NA },
               sch_ela_cg = if ("School Closing Gaps Score" %in% colnames(rc)) {
                 `School ELA Gap Score`
               } else { NA },
               sch_math_cg = if ("SChool Closing Gaps Score" %in% colnames(rc)) {
                 `School Mathematics Gap Score`
               } else { NA },
               
               # These fields don't exist in all files, so will throw an error
               # without using conditional.
               
               sch_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Outcomes Score`
               } else { NA },
               sch_ach_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Achievement Score`
               } else { NA },
               sch_growth_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Growth Score`
               } else { NA },
               sch_absenteeism_tgo =if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Chronic Absenteeism Score`
               } else { NA },
               sch_graduation_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Graduation Score`
               } else { NA },
               sch_attendance_tgo = if ("School Target Group Outcomes Score" %in% colnames(rc)) {
                 `School Target Group Attendance Score`
               } else { NA },
               sch_4y_grad_gap = if("School 4 Year Graduation Gap Score" %in% colnames(rc)) {
                 `School 4 Year Graduation Gap Score`} else {NA},
               sch_6y_grad_gap = if("School 6 Year Graduation Gap Score" %in% colnames(rc)) {
                 `School 6 Year Graduation Gap Score`} else {NA},
               sch_grad_gap = if ("School Graduation Gap Score" %in% colnames(rc)) {
                 `School Graduation Gap Score`
               } else { NA },
               sch_ot = if ("School On-Track and Postsecondary Readiness Score" %in% colnames(rc)) {
                 `School On-Track and Postsecondary Readiness Score`
               } else { `School On-Track to Graduation Score` },
               sch_grad_rate = `School Graduation Rate Score`,
               sch_att_rate = `School Attendance Rate Score`,
               sch_3rd_ela = if ("School Third-Grade ELA Achievement Score" %in% colnames(rc)) {
                 `School Third-Grade ELA Achievement Score`
               } else if ("School 3rd Grade ELA Achievement Score" %in% colnames(rc)) {
                 `School 3rd Grade ELA Achievement Score`
               } else { NA },
               sch_8th_math = if ("School Eighth-Grade Mathematics Achievement Score" %in% colnames(rc)) {
                 `School Eighth-Grade Mathematics Achievement Score`
               } else if ("School 8th Grade Mathematics Achievement Score" %in% colnames(rc)) {
                 `School 8th Grade Mathematics Achievement Score`
               } else { NA },
               ach_weight = if("Score weighting Achievement Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Achievement Priority Area`)} else {NA},
               growth_weight = if("Score weighting Growth Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Growth Priority Area`)} else {NA},
               cg_weight = if("Score weighting Closing Gaps Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Closing Gaps Priority Area`)} else {NA},
               tgo_weight = if("Score weighting Target Group Outcomes Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Target Group Outcomes Priority Area`)} else {NA},
               ot_weight = if("Score weighting Ontrack Priority Area" %in% colnames(rc)) {
                 as.numeric(`Score weighting Ontrack Priority Area`)} else if (
                   "Score weighting On-Track Priority Area" %in% colnames(rc)
                 ) {
                   as.numeric(`Score weighting On-Track Priority Area`)}
               else {NA},
               locale_description = `Locale description`,
               city = City,
               report_card_type = if("Report Card Type" %in% colnames(rc)) {
                 `Report Card Type`} else {NA},
               district_code = gsub("NA", 0000, district_code),
               district_code = str_pad(district_code, 4, side = "left", pad = 0),
               school_code = str_replace(school_code, "[:alpha:]", ""),
               school_code = str_pad(school_code, 4, side = "left", pad = 0),
               dpi_true_id = paste(district_code, school_code, sep = "_"),
               overall_score = round(as.numeric(overall_score), 1),
               school_enrollment = as.numeric(school_enrollment),
               district_enrollment = as.numeric(district_enrollment),
               cg_redacted = ifelse(str_detect(sch_cg, "\\*"), 1, 0),
               test_participation_ela = if ("Test Participation ELA 2021 All Students" %in% colnames(rc)) {
                 as.numeric(`Test Participation ELA 2021 All Students`)
               } else { NA },
               test_participation_math = if ("Test Participation Mathematics 2021 All Students" %in% colnames(rc)) {
                 as.numeric(`Test Participation Mathematics 2021 All Students`)
               } else { NA }) %>%
        mutate_at(vars("per_am_in":"per_open"), funs(round(as.numeric(.), 3))) %>%
        mutate_at(vars("sch_ach":"sch_8th_math"), funs(round(as.numeric(.), 1))) %>%
        
        # Since `mutate` was used above, columns were created instead of
        # being renamed.  Using select drops all original columns and leaves
        # just those we are interested in; also, using column names facilitates
        # functionality across files where column indices might differ.
        
        select(school_year,
               dpi_true_id,
               district_name,
               school_name,
               overall_score,
               overall_rating,
               lowest_grade,
               highest_grade,
               grade_band,
               school_type,
               school_enrollment,
               district_enrollment,
               per_am_in,
               per_asian,
               per_b_aa,
               per_hisp_lat,
               per_nh_opi,
               per_white,
               per_tom,
               per_swd,
               per_ed,
               per_lep,
               per_choice,
               per_open,
               sch_ach,
               sch_ela_ach,
               sch_math_ach,
               sch_growth,
               sch_ela_growth,
               sch_math_growth,
               sch_cg,
               sch_ela_cg,
               sch_math_cg,
               sch_tgo,
               sch_ach_tgo,
               sch_growth_tgo,
               sch_absenteeism_tgo,
               sch_graduation_tgo,
               sch_attendance_tgo,
               sch_4y_grad_gap,
               sch_6y_grad_gap,
               sch_grad_gap,
               sch_ot,
               sch_grad_rate,
               sch_att_rate,
               sch_3rd_ela,
               sch_8th_math,
               ach_weight,
               growth_weight,
               cg_weight,
               tgo_weight,
               ot_weight,
               locale_description,
               city,
               report_card_type,
               cg_redacted,
               test_participation_ela,
               test_participation_math)
      
    }
    rc_renamed <- bind_rows(rc_renamed, rc_other)
  }
  
  # Add variable to indicate when two report cards exist for
  # a private school in a year.
  
  mult_rc <- rc_renamed %>%
    group_by(dpi_true_id, school_year) %>%
    summarise(number = n()) %>%
    mutate(has_2_rc = ifelse(number == 2, 1, 0)) %>%
    select(-number)
  
  rc_renamed <- left_join(rc_renamed, mult_rc, by = c("dpi_true_id", "school_year"))
  
  # Fix Excel grade band to date madness
  
  rc_renamed <- rc_renamed %>%
    mutate(grade_band = case_when(grade_band == "42898" ~ "6-12",
                                  grade_band == "42990" ~ "9-12",
                                  grade_band == "42894" ~ "6-8",
                                  grade_band == "43628" ~ "6-12",
                                  grade_band == "43624" ~ "6-8",
                                  grade_band == "43720" ~ "9-12",
                                  TRUE ~ grade_band))
  
  return(rc_renamed)
}

rc_renamed <- make_report_cards()

