# School report
library(tidyverse)

# District CDS for report
district_cds <- "3367181"
district_name <- "Palo Verde"
curr_county <- "33"

# Import entity list
load('cache/entities.Rdata')
load(paste0('cache/caaspp_long_',curr_county,'.Rdata'))

# Make school list
schools <- entities %>%
  filter(substr(CDS, 1,7) == district_cds & substr(CDS,8,14) != "0000000")

# Create directory
if(!dir.exists(paste0("Schools/", district_name))) {
  dir.create(paste0("Schools/", district_name))
}

# School reports
for(i in 1:nrow(schools)) {
  school_cds = schools[i,] %>% pull(CDS)
  school_name = schools[i,] %>% pull(school)
  
  curr_school_caaspp <- curr_caaspp_long %>%
    filter(CDS == school_cds)
  if (nrow(curr_school_caaspp) == 0) {
    next
  }
  
  rmarkdown::render(
    'county_report.Rmd',
    output_file = paste0("Schools/", district_name, "/", school_name, " Report.pdf"),
    params = list(
      cds = school_cds,
      dist_name = school_name,
      include_others = FALSE
    )
  )
}
