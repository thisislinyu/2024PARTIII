library(readr)
library(stringr)
library(poLCA)
library(ggplot2)
library(dplyr)

cope_subset <- read_csv("data/cope_subset.csv")
dem_orient_subset <- cope_subset %>% dplyr::select(b_dem_orientation)

recode_orient_dat <- dem_orient_subset %>%
mutate(recod_orient= case_when(
  b_dem_orientation %in% c("Asexual", "I do not use a label", "Other/Not listed (please specify)") ~ "Other",
  b_dem_orientation %in% c("Gay/Lesbian/Homosexual", "Queer", "Unsure/Questioning") ~ "LGBTQ",
  b_dem_orientation %in% c("Bisexual", "Pansexual") ~ "LGBTQ",
  b_dem_orientation == "Heterosexual/Straight" ~ "Heterosexual",
  TRUE ~ b_dem_orientation
))


recode_orient_dat$recod_orient %>% table()
