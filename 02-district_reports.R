library(tidyverse)

districts <- read_delim('https://www.cde.ca.gov/schooldirectory/report?rid=dl2&tp=txt') %>%
  mutate(CDS = paste0(`CD Code`, "0000000")) %>%
  select(CDS, district = District) %>%
  mutate(
    district = gsub("\\s*\\w*$", "", district),
    district = gsub("of$", "of Education", district)
  )

riv_dists <- districts %>%
  filter(
    substr(CDS, 1, 2) == "33",
    CDS != "33316250000000",
    CDS != "33744920000000",
    CDS != "33670410000000"
  )

for(i in 1:nrow(riv_dists)) {
  cds = riv_dists[i,] %>% pull(CDS)
  name = riv_dists[i,] %>% pull(district)
  rmarkdown::render(
    'county_report.Rmd',
    output_file = paste0("Districts/", name, " Report.pdf"),
    params = list(
      cds = cds,
      dist_name = name,
      include_others = FALSE
    )
  )
}
