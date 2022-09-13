library(tidyverse)
library(ggthemes)
library(ggrepel)

c2015 <- read_csv("data/sb_ca2015_all_csv_v3.txt", 
                  na = c("","*"), 
                  col_types = cols(
                    `County Code` = col_character(),
                    Filler = col_character()
                    )
                  ) 

c2016 <- read_csv("data/sb_ca2016_all_csv_v3.txt", 
                  na = c("","*"), 
                  col_types = cols(
                    `County Code` = col_character(),
                    Filler = col_character()
                    )
                  ) 

c2017 <- read_csv("data/sb_ca2017_all_csv_v2.txt", 
                  na = c("","*"), 
                  col_types = cols(
                    `County Code` = col_character(),
                    Filler = col_character()
                    )
                  ) 

c2018 <- read_csv("data/sb_ca2018_all_csv_v3.txt", 
                  na = c("","*"), 
                  col_types = cols(
                    `County Code` = col_character(),
                    Filler = col_character()
                    )
                  ) 

c2019 <- read_csv("data/sb_ca2019_all_csv_v4.txt",
                  na = c("","*"), 
                  col_types = cols(
                    `County Code` = col_character(),
                    Filler = col_character()
                    )
                  ) 

c2022 <- read_delim("data/sb_ca2022_all_csv_v1.txt", 
                    na = c("","*"), 
                    delim = "^"
                    ) %>%
  rename("Subgroup ID" = "Student Group ID",
         "Test Id" = "Test ID") 

entities <- read_delim('https://www.cde.ca.gov/schooldirectory/report?rid=dl1&tp=txt') %>%
  select(CDS = CDSCode, district = District, school = School, DOC, DOCType, SOC, SOCType) %>%
  mutate(
    district = gsub("\\s*\\w*$", "", district),
    district = gsub("of$", "of Education", district)
  )

groups <- read_delim('data/StudentGroups.txt', delim = "^")

focus_groups <- groups %>%
  rename(
    `group` = `Demographic ID Num`
  ) %>%
  filter(
    `group` %in% c(1, 128, 31, 160, 8, 75, 76, 74, 78, 78, 79, 80, 144, 4, 3, 28, 52, 240)
  ) %>%
  bind_cols(short_group = c(
  "ALL",
  "SWD",
  "SED",
  "RFEP",
  "EL",
  "AI",
  "AS",
  "AA",
  "HIS",
  "PI",
  "WH",
  "MR",
  "FEM",
  "MAL",
  "MIG",
  "HOM",
  "FOS"
))

test_id <- tibble(
  TestID = c(1,2),
  `Test Type` = c("ELA","Math")
)

caaspp <- bind_rows(c2015, c2016, c2017, c2018, c2019, c2022) %>%
  mutate(CDS = paste0(`County Code`, `District Code`, `School Code`)) %>%
  select(
    CDS,
    year = `Test Year`,
    Grade,
    group = `Subgroup ID`,
    TestID = `Test Id`,
    perc = `Percentage Standard Met and Above`,
    `Exceeded` = `Percentage Standard Exceeded`,
    `Met` = `Percentage Standard Met`,
    `Nearly Met` = `Percentage Standard Nearly Met`,
    `Not Met` = `Percentage Standard Not Met`,
    `Mean Scale Score`,
    n = `Students with Scores`
  ) %>%
  left_join(entities) %>%
  inner_join(focus_groups) %>%
  left_join(test_id) %>%
  mutate(
    `Nearly Met` = -`Nearly Met`,
    `Not Met` = -`Not Met`,
    year = as.character(year),
    Grade = as.character(Grade),
    Grade = if_else(Grade == "13", "All", Grade),
    district = 
      case_when(
        district == "Riverside County Office of Education" ~ "RCOE", 
        CDS == "33000000000000" ~ "Riverside County",
        TRUE ~ district)
  )

caaspp_long <- caaspp %>%
  select(-perc) %>%
  pivot_longer(
    cols = `Exceeded`:`Not Met`,
    names_to = "Level",
    values_to = "Percent"
  ) %>%
  mutate(Level = factor(Level, c("Exceeded", "Met", "Not Met", "Nearly Met")),
         Grade = factor(Grade, c("All","3","4","5","6","7","8","11")),
         year = factor(year, c("2015", "2016", "2017", "2018", "2019", "2022")),
         cohort = if_else(
           Grade == "All", 
           "", 
           paste0("Class of ", as.character(as.numeric(as.character(year)) + 12 - as.numeric(as.character(Grade))
                        ))))

race_ethnicity = c(1,75,76,74,77,78,79,80,144)
program = c(1,31,160,240,52,128,4,3)

# Create DFS file

ela_targets_original <- tibble(
  TestID = 1,
  Grade = c("3","4","5","6","7","8","11"),
  ss_target = c(2432,2473,2502,2531,2552,2567,2583)
)

math_targets_original <- tibble(
  TestID = 2,
  Grade = c("3","4","5","6","7","8","11"),
  ss_target = c(2436,2485,2528,2552,2567,2586,2628)
)

ela_targets_new <- tibble(
  TestID = 1,
  Grade = c("3","4","5","6","7","8","11"),
  ss_target = c(2432,2473,2502,2531,2552,2567,2583)
)

math_targets_new <- tibble(
  TestID = 2,
  Grade = c("3","4","5","6","7","8","11"),
  ss_target = c(2436,2485,2528,2552,2567,2586,2628)
)

original_years = factor(c("2015", "2016", "2017", "2018", "2019", "2020"))

final_targets <- bind_rows(
  crossing(year = original_years, ela_targets_original),
  crossing(year = original_years, math_targets_original),
  crossing(year = "2022", ela_targets_new),
  crossing(year = "2022", math_targets_new)
)

years <- tibble(
  year = c("2015", "2016", "2017", "2018", "2019", "2022"),
  prev_year = c(NA, "2015", "2016", "2017", "2018", "2019")
)

caaspp_dfs <- caaspp_long %>%
  select(-Level, -Percent) %>%
  filter(!is.na(`Mean Scale Score`)) %>%
  unique() %>%
  group_by(CDS, year, school, `Demographic Name`, `Student Group`, `Test Type`, group, short_group, TestID) %>%
  left_join(final_targets) %>%
  mutate(dfs = `Mean Scale Score` - ss_target,
         weighted_dfs = dfs * n) %>%
  summarize(
    n = sum(n),
    weighted_dfs = sum(weighted_dfs),
    dfs = weighted_dfs / n,
    grade_levels_included = n()
  ) %>%
  left_join(years)%>%
  ungroup()

prev_caasspp <- caaspp_dfs %>% select(prev_year = year, prev_dfs = dfs, CDS, group, TestID) %>% filter(!is.na(prev_year))
  
caaspp_dfs2 <- caaspp_dfs %>%
  left_join(prev_caasspp) %>%
  mutate(change = dfs - prev_dfs) %>%
  mutate(dfs = if_else(
    grade_levels_included >= 4 | !(is.na(school) | school == "No data"), 
    dfs, 
    NA_real_)) 

caaspp_dfs3 <- caaspp_dfs2 %>%
  left_join(entities) %>%
  mutate(
    district = 
      case_when(
        district == "Riverside County Office of Education" ~ "RCOE", 
        CDS == "33000000000000" ~ "Riverside County",
        TRUE ~ district)
  )

caaspp_dfs <- caaspp_dfs3

# The KEY function for plotting dfs


plot_dfs <- function(data, x_axis, test_id, dodge) {
  # Completes the grouping to allow for blank columns
  if(deparse(substitute(x_axis)) != "year") {
    data <- data %>% complete({{x_axis}}, year)
  }
  # Wraps the text for column titles if longer than 15 characters
  if(is.factor(data %>% pull({{x_axis}}))) {  
    x_axis_levels <- levels(data %>% pull({{x_axis}}))
    wrapped_levels <- str_wrap(x_axis_levels, width = 15)
    data <- data %>%
      mutate("{{x_axis}}" := factor(str_wrap({{x_axis}}, width = 15), levels = wrapped_levels))
  } else {
    data <- data %>%
      mutate("{{x_axis}}" := str_wrap({{x_axis}}, width = 15))
  }
  
  # Start the plot  
  plot <- ggplot(data, aes(x={{x_axis}}, y=dfs, fill = year))

  # Determine if HS cutpoints needed for DFS
  SOC = data[1,] %>% pull(SOC)
  DOC = data[1,] %>% pull(DOC)
  
  # DOC 56 = High school district, SOC 66,67,68 are high schools
  if (SOC == "No Data" | is.na(SOC)) {
    hs_cutpoints = if_else(is.na(DOC) | DOC != 56, FALSE, TRUE)
  } else {
    hs_cutpoints = if_else(
      SOC %in% c(66, 67, 68),
      TRUE,
      FALSE
    )
  }
  
  # Add cut points for levels
  if(test_id == 1 & hs_cutpoints == FALSE) {
    plot <- plot + 
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax=-70 , fill = "red", alpha = 0.5) +
      geom_hline(yintercept = 45, linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = 10,linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = -5, linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = -70, linetype = "longdash", alpha = 0.3) 
  } else if(test_id == 1 & hs_cutpoints == TRUE) {
    plot <- plot + 
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax=-45 , fill = "red", alpha = 0.5) +
      geom_hline(yintercept = 75, linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = 30,linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = -45, linetype = "longdash", alpha = 0.3) 
  } else if (test_id == 2 & hs_cutpoints == FALSE) {
    plot <- plot + 
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax=-95 , fill = "red", alpha = 0.5) +
      geom_hline(yintercept = 35, linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = 0,linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = -25, linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = -95, linetype = "longdash", alpha = 0.3) 
  } else if (test_id == 2 & hs_cutpoints == TRUE) {
    plot <- plot + 
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax=-115 , fill = "red", alpha = 0.5) +
      geom_hline(yintercept = 25, linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = 0,linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = -60, linetype = "longdash", alpha = 0.3) +
      geom_hline(yintercept = -115, linetype = "longdash", alpha = 0.3) 
  }
  
  # Final plot function
  plot <- plot +  
      geom_col(position = position_dodge(preserve = "single")) + # Make a column plot
      geom_text(aes(y = dfs + (sign(dfs) * 4), label = round(dfs,1)), size = 3, position = position_dodge(width = .9))+ # Adds the labels
      scale_fill_economist() + # Use the economist color theme
      theme_few() + # Make things more minimalistic theme wise
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
      geom_hline(yintercept = 0) +
    labs(caption = "Note: Groups may be suppressed due to grade level n-size that will have data on the Fall 2022 CA School Dashboard")
  return(plot)
}

#
plot_data <- caaspp_dfs3 %>%
  filter(
    year %in% c("2019", "2022"),
    CDS == "33671813335759",
    TestID == 1,
    group %in% program
  )
plot <- plot_dfs(plot_data, x_axis = `Demographic Name`, test_id = 1) +
  ggtitle("2022 Smarter Balanced Assessment Results", subtitle = "Student Race/Ethnicity Distance from Standard: English Language Arts")
plot

plot_data <- caaspp_dfs3 %>%
  filter(
    # year == "2022",
    CDS == "33671813335759",
    TestID == 1,
    group == 1,
  )
plot <- plot_dfs(plot_data, x_axis = `year`, test_id = 1) +
  ggtitle("2022 Smarter Balanced Assessment Results", subtitle = " CAASPP DFS Over Time: English Language Arts") +
  theme(legend.position = "none")
plot

# Plot CAASPP Function

plot_caaspp <- function(
    data,
    x_axis,
    facet) {
  # Define the plot colors
  colors = c("#33ADD8", "#91CD69", "#DFE553","#F9AD86" )
  
  # Wrap the legend text
  if(is.factor(data %>% pull({{x_axis}}))) {  
    x_axis_levels <- levels(data %>% pull({{x_axis}}))
    wrapped_levels <- str_wrap(x_axis_levels, width = 20)
    data <- data %>%
      mutate("{{x_axis}}" := factor(str_wrap({{x_axis}}, width = 20), levels = wrapped_levels))
  } else {
    data <- data %>%
      mutate("{{x_axis}}" := str_wrap({{x_axis}}, width = 20))
  }
  # Calculate the percent proficient by group
  
  if(!is.null(facet)) {
    
    data_proficient <- data %>%
      filter(Level %in% c("Met","Exceeded")) %>%
      group_by({{x_axis}}, {{facet}}) %>%
      summarise(Percent = round(sum(Percent),0))
    
    plot <- ggplot(
      data, 
      aes(x = {{x_axis}}, y = Percent, fill = Level)) +
      geom_col() +
      theme_few() +
      scale_fill_manual(values = colors, breaks = c("Exceeded", "Met","Nearly Met" , "Not Met" )) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      facet_grid(cols = vars({{facet}})) 
    
    if(nrow(data_proficient) >= 15) {
      plot <- plot +
        geom_text(data = data_proficient, aes(x={{x_axis}}, y = Percent + 5, label = paste0(Percent, "%"), angle = 90), inherit.aes = FALSE)
    } else {
      plot <- plot +
        geom_text(data = data_proficient, aes(x={{x_axis}}, y = Percent + 5, label = paste0(Percent, "%")), inherit.aes = FALSE)
    }
  } else {
    data_proficient <- data %>%
      filter(Level %in% c("Met","Exceeded")) %>%
      group_by({{x_axis}}) %>%
      summarise(Percent = round(sum(Percent),0))
    
    plot <- ggplot(
      data, 
      aes(x = {{x_axis}}, y = Percent, fill = Level)) +
      geom_col() +
      theme_few() +
      scale_fill_manual(values = colors, breaks = c("Exceeded", "Met","Nearly Met" , "Not Met" )) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      geom_text(data = data_proficient, aes(x={{x_axis}}, y = Percent + 5, label = paste0(Percent, "%")), inherit.aes = FALSE)
  }
return(plot)
}

# 5x5 plot
cut_scores <- read_csv("cutscores.csv") %>%
  mutate(
    across(c(xmin,ymin), ~ if_else(is.na(.x), -Inf, .x))
  ) %>% 
  mutate(
    across(c(xmax,ymax), ~ if_else(is.na(.x), Inf, .x))
  ) %>% left_join(
    tibble(
      color = 1:5,
      hex =  c("#a2060a","#fca526","#fffe37","#00640c","#0202fb")
  )
  )
plot_5x5 <- function(data, grid) {
    boxes <- cut_scores %>% filter(indicator == grid)
    rect_plot <- ggplot() + 
      geom_rect(
        data = boxes,
        aes(xmin = xmin, xmax = xmax, ymin= ymin, ymax = ymax, fill = I(hex)), 
        alpha = 0.7,
        color = "black",
        show.legend = FALSE,
        inherit.aes = FALSE) +
      lims(x = c(-30,30), y = c(-120,70)) + 
      geom_point(data = data, aes(x = change, y = dfs)) +
      geom_text_repel(data = data, aes(x = change, y = dfs, label = short_group)) +
      theme_few()
  return(rect_plot)
}

# Test five by five
five_data <- caaspp_dfs %>%
  filter(
    CDS == "33000000000000",
    year == "2022",
    TestID == 1
  )
five_plot <- plot_5x5(five_data, grid = "ela")
five_plot

# Save the resulting files
## This saves datasets only
for (i in 1:58) {
  county_code <- str_pad(i, 2, pad = "0")
  curr_caaspp_long <- caaspp_long %>%
    filter(substr(CDS,1,2) == county_code)
  save(curr_caaspp_long, file = paste0('cache/caaspp_long_',county_code,'.Rdata'))
}
save(caaspp_long, file = 'cache/caaspp_long.Rdata')
save(caaspp_dfs, file = 'cache/caaspp_dfs.Rdata')
save(cut_scores, file = 'cache/cut_scores.Rdata')
save(race_ethnicity, file = "cache/race_ethnicity.Rdata")
save(program, file = "cache/program.Rdata")
save(entities, file = "cache/entities.Rdata")

## Also save the R functions that are way too long
save(plot_dfs, file = "cache/plot_dfs.Rdata")
save(plot_5x5, file = "cache/plot_5x5.Rdata")
save(plot_caaspp, file = "cache/plot_caaspp.Rdata")


# Test it!

test_ela_multiyear <- caaspp_long %>%
  filter(
    # year == 2022,
    CDS == "33000000000000",
    # TestID == 1,
    group == 1,
    Grade == "All"
  )
test_plot <- plot_caaspp(test_ela_multiyear, x_axis = year, facet = expr(`Test Type`))
test_plot

# Test student group

plot_data <- caaspp_long %>%
  filter(
    year == "2022",
    CDS == "33000000000000",
    TestID == 1,
    group %in% race_ethnicity,
    Grade == "All"
  )
plot <- plot_caaspp(plot_data, x_axis = `Demographic Name`, facet = NULL) +
  ggtitle("2022 Smarter Balanced Assessment Results", subtitle = "Student Race/Ethnicity Comparison: English Language Arts")
plot

