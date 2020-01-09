library(tidyverse)


choice <- readxl::read_xls("C:/Users/Spencer/Downloads/2018-19_summary_mpcp_wpcp_rpcp_snsp.xls", skip = 4) %>%
  filter(!is.na(`...1`))

snames <- schools %>% select(dpi_true_id, school_name)
gnames <- graduation %>% select(dpi_true_id, school_name) %>% unique()
rcnames <- report_cards %>% select(dpi_true_id, school_name) %>% unique()
fnames <- choice_forward %>% select(dpi_true_id, school_name) %>% unique()

names <- bind_rows(snames, gnames, rcnames, fnames) %>% unique()


fuzzy <- map(choice$`School Name`, agrep, x = names$school_name)


for(i in 1:nrow(choice)) { 
  
  if(length(fuzzy[[i]]) > 0) {
    
    choice$guess[i] <- names[fuzzy[[i]], 2]
    
  } else {
    choice$guess[i] <- NA
  }

      
}

    
sum(is.na(choice$guess))
