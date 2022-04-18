library(tidyverse)
library(wisconsink12)
library(glue)

school_years <- unique(schools$school_year)

walk(school_years, function(x) {
  tmp <- forward_exam %>%
    filter(school_year == x)
  write_csv(tmp, glue("data/forward_exam_{x}.csv"))
})
