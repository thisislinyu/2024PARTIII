library(readr)
library(stringr)
library(poLCA)
library(ggplot2)
library(dplyr)

cope_subset <- read_csv("data/cope_subset.csv")
dem_race_subset <- cope_subset %>% dplyr::select(starts_with("b_dem_race"))

race_cor <- cor(dem_race_subset) %>% data.frame()

### white 000010

case_when(race_pattern=="000010"~ 'white')

tmp <- dem_race_subset %>%
  mutate(race_pattern = apply(., 1, function(row) paste0(row, collapse = ""))) %>%
  cbind(cope_subset$b_response_id) %>%
  mutate(recode_race = case_when(
    race_pattern == "000010" ~ 'White',
    race_pattern == "000001" ~ 'Black/African-American',
    race_pattern == "010000" ~ 'Asian Including Asian Desi',
    race_pattern == "001000" ~ 'Hispanic/Latinx',
    race_pattern == "000000" ~ 'Prefer not to answer',
    TRUE ~ 'Mixed'
  ))





