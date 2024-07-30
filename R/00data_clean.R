## read dataset------
# source('R/helper.R')
# load("E:/2022/IRP/R_project/data/glmnet_m0517111.rds")

library(readr)
library(gtsummary)
library(flextable)
library(officer)
cope_subset <- read_csv("data/cope_subset.csv")


dem_race_subset <- cope_subset %>% dplyr::select(starts_with("b_dem_race"))

race_cor <- cor(dem_race_subset) %>% data.frame()

### white 000010

case_when(race_pattern=="000010"~ 'white')

race_pattern_dat <- cope_subset %>%
  dplyr::select(starts_with("b_dem_race")) %>%
  mutate(race_pattern = apply(., 1, function(row) paste0(row, collapse = "")))


cope_subset1 <- cope_subset %>%
  mutate(race_pattern =race_pattern_dat$race_pattern ) %>%
  mutate(recode_race = case_when(
    race_pattern == "000010" ~ 'White',
    race_pattern == "000001" ~ 'Black/African-American',
    race_pattern == "010000" ~ 'Asian Including Asian Desi',
    race_pattern == "001000" ~ 'Hispanic/Latinx',
    race_pattern == "000000" ~ 'Prefer not to answer',
    TRUE ~ 'Mixed'
  )) %>%
  dplyr::select(-race_pattern) %>%
  mutate(recode_language = case_when(
    b_dem_language == "English" ~ 'English',
    TRUE ~ 'Other'
  )) %>%
  dplyr::select(-b_dem_language,-starts_with("b_dem_race")) %>%
  select(recode_race,starts_with("b_dem_gender"),b_screener_age,b_dem_sex,b_dem_orientation,
         starts_with("b_covid_family"), starts_with("b_covid_cope"),recode_language, everything()
         )



## table one


table1 <- tbl_summary(cope_subset1 %>% select(-b_response_id,-f1_cdi_mean),
                      by = condition, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                        all_categorical() ~ "{n} ({p}%)"), digits = all_continuous() ~ 2,
                      label = list(
                        #b_dem_race_american_indian_or_alaska_native ~ "American Indian or Alaska Native",
                                #   b_dem_race_asian_including_asian_desi ~ "Asian Including Asian Desi",
                                 #  b_dem_race_hispanic_latinx ~ "Hispanic/Latinx",
                                 #  b_dem_race_native_hawaiian_or_other_pacific_islander ~ "Native Hawaiian or Other Pacific Islander",
                                 #  b_dem_race_white_caucasian_non_hispanic_includes_middle_eastern ~ "White",
                                  # b_dem_race_black_african_american ~"Black/African-American",
                                 #  b_dem_race_other_specify ~ "Other",
                                 #  b_dem_race_prefer_not_to_answer ~ "Prefer Not to Answer",
                                   b_dem_gender_agender ~ "Agender",
                                   b_dem_gender_not_sure ~ "Not sure/Questioning",
                                   b_dem_gender_other_please_specify ~ "Unspecified Gender",
                                   b_dem_gender_androgynous ~ "Androgynous",
                                   b_dem_gender_nonbinary ~ "Non-binary",
                                   b_dem_gender_two_spirited ~ "Two-spirited",
                                   b_dem_gender_female_to_male_transgender_ftm ~ "Transgender - Female to Male",
                                   b_dem_gender_trans_female_trans_feminine ~ "Trans Female/Trans Feminine",
                                   b_dem_gender_trans_male_trans_masculine ~ "Trans Male/Trans Masculine",
                                   b_dem_gender_gender_expansive ~ "Gender Expansive",
                                   b_dem_gender_third_gender ~ "Third Gender",
                                   b_dem_gender_genderqueer ~ "Genderqueer",
                                   b_dem_gender_male_to_female_transgender_mtf ~ "Transgender - Male to Female",
                                   b_dem_gender_man_boy ~ "Man/Boy",
                                   b_dem_gender_transgender ~ "Transgender",
                                   b_dem_gender_woman_girl ~ "Woman/Girl",
                                   b_dem_sex ~ "Biological Sex",
                                   b_dem_orientation ~ "Sexual Orientation"
                                   #,
                                   # b_cdi_sum ~ "Baseline CDI Sum Score (0-24)"
                                   )
                      ) %>%
  modify_header(label ~ "**Demographics**") %>%
  modify_spanning_header(c("stat_1", "stat_2","stat_3") ~ "**Treatment Received**")

table1


tmp <- cope_subset$b_dem_orientation %>% table(useNA = "always") %>% data.frame()
###

cope_subset %>% select(starts_with("b_dem_race")) %>%
 apply(.,1,sum) %>% table()

tmp1 <- apply(tmp,1,sum) %>% data.frame()

table(tmp1$.==0)




## check the correlation


## Convert to flextable so we can output to Word

table1_word <- table1 %>%
  as_flex_table()

## Creating table one in a Word Doc format

read_docx() %>%
  body_add_flextable(value = table1_word) %>%
  print(target = "cope_table_1_word.docx")


table1_df <- as.data.frame(table1)

library(xtable)
latex_table1 <- xtable(table1_df)



writeLines(print(latex_table1, type = "latex"), "table1_df.tex")



cope_subset2 <- cope_subset1

cope_subset2$genderid3 <- genderid_class

cope_subset2$cope3 <- cope_class




cope_subset3 <- cope_subset2 %>%
  mutate(family_num =challenge_num_dat$challenge_num3,
         family_cat = challenge_cat_dat$challenge_cat) %>%
  mutate(orientation = recode_orient_dat$recod_orient) %>%
  select(c("b_response_id", "condition", "b_cdi_mean", "recode_race",
           #"b_dem_gender_agender", "b_dem_gender_not_sure",
           #"b_dem_gender_other_please_specify", "b_dem_gender_androgynous",
           #"b_dem_gender_nonbinary", "b_dem_gender_two_spirited", "b_dem_gender_female_to_male_transgender_ftm",
           #"b_dem_gender_trans_female_trans_feminine", "b_dem_gender_trans_male_trans_masculine",
           #"b_dem_gender_gender_expansive", "b_dem_gender_third_gender",
           #"b_dem_gender_genderqueer", "b_dem_gender_male_to_female_transgender_mtf",
          # "b_dem_gender_man_boy", "b_dem_gender_transgender", "b_dem_gender_woman_girl",
           "b_screener_age", "b_dem_sex", "orientation",
            #"b_covid_family_family_did_not_enough_enough_money_for_food",
           #"b_covid_family_family_did_not_have_a_regular_place_to_sleep_or_stay",
           #"b_covid_family_i_could_not_attend_school_in_person", "b_covid_family_i_could_not_attend_school_at_all",
           #"b_covid_family_other", "b_covid_family_family_did_not_have_enough_money_for_gas_transportation",
           #"b_covid_family_family_did_not_have_enough_money_to_pay_rent",
           #"b_covid_family_the_covid_19_pandemic_has_not_affected_me_or_my_family_in_these_ways_in_the_past_2_weeks",
           #"b_covid_cope_1_connecting_with_others", "b_covid_cope_1_including_talking_with_people_you_trust_about_your_concerns_and_how_you_are_feeling",
           #"b_covid_cope_1_contacting_a_healthcare_provider", "b_covid_cope_1_drinking_alcohol",
           #"b_covid_cope_1_smoking_more_cigarettes_or_vaping_more",
           "recode_language",
           "f1_cdi_mean", "genderid3","family_num","family_cat",
           # "family3",
          "cope3"))




dat_cc <- cope_subset3 %>%
  filter(recode_race!="Prefer not to answer") %>%  ## 1468
  filter(b_dem_sex!="Prefer not to say" & b_dem_sex!="Other") %>% ## 1447
  filter(orientation!="I do not want to respond")%>%  ## 1441
  mutate(
   # condition = factor(condition),
    recode_race = factor(recode_race),
    b_screener_age = factor(b_screener_age),
    b_dem_sex = factor(b_dem_sex),
    orientation = factor(orientation),
    genderid3 = factor(genderid3),
    family_num = factor(family_num),
    family_cat = factor(family_cat),
    cope3 = factor(cope3)
  )

save(dat_cc,file="data/dat_cc.rds")

















