# CHAPTER 220 ====
# Uses Sept. Numbers
# Source: https://dpi.wi.gov/sfs/aid/general/integration-220/overview

library(tidyverse)

school_year <- c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20")
total_enrollment <- c(1266 , 1038, 842, 672, 536)

chapter_220 <- tibble(school_year, total_enrollment, 
                      accurate_agency_type = "Chapter 220",
                      broad_agency_type = "Open Enrollment/Chapter 220")
