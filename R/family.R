
library(readr)
library(stringr)
library(poLCA)
library(ggplot2)
library(dplyr)

cope_subset <- read_csv("data/cope_subset.csv")
dem_family_subset <- cope_subset %>% dplyr::select(starts_with("b_covid_family"))

familly_cor <- cor(dem_family_subset) %>% data.frame()


challenge_num_dat <- dem_family_subset %>%
  mutate(challenge_num = apply(., 1, sum)) %>%
  mutate(challenge_num = ifelse(b_covid_family_the_covid_19_pandemic_has_not_affected_me_or_my_family_in_these_ways_in_the_past_2_weeks=="1",0,challenge_num)) %>%
  mutate(challenge_num3 = ifelse(challenge_num>=2, 2, challenge_num))


challenge_cat_dat <-  dem_family_subset %>%
  mutate(challenge_cat = case_when(
    b_covid_family_family_did_not_enough_enough_money_for_food == 1 |
      b_covid_family_family_did_not_have_enough_money_for_gas_transportation == 1 |
      b_covid_family_family_did_not_have_a_regular_place_to_sleep_or_stay==1 |
      b_covid_family_family_did_not_have_enough_money_to_pay_rent == 1 ~ "Financial",

    b_covid_family_i_could_not_attend_school_in_person == 1 |
      b_covid_family_i_could_not_attend_school_at_all == 1 ~ "School",

    b_covid_family_other == 1 ~ "Other",

    b_covid_family_the_covid_19_pandemic_has_not_affected_me_or_my_family_in_these_ways_in_the_past_2_weeks == 1 ~ "No impact",

    TRUE ~ "No"
  ))


challenge_cat_dat <- dem_family_subset %>%
  mutate(
    Financial = b_covid_family_family_did_not_enough_enough_money_for_food == 1 |
      b_covid_family_family_did_not_have_enough_money_for_gas_transportation == 1 |
      b_covid_family_family_did_not_have_a_regular_place_to_sleep_or_stay == 1 |
      b_covid_family_family_did_not_have_enough_money_to_pay_rent == 1,

    School = b_covid_family_i_could_not_attend_school_in_person == 1 |
      b_covid_family_i_could_not_attend_school_at_all == 1,

    Other = b_covid_family_other == 1,

    No_impact = b_covid_family_the_covid_19_pandemic_has_not_affected_me_or_my_family_in_these_ways_in_the_past_2_weeks == 1,

    challenge_cat = case_when(
      Financial & School & Other ~ "Other",
      Financial & School ~ "Other",
      Financial & Other ~ "Other",
      School & Other ~ "Other",
      Financial ~ "Other",
      School ~ "School",
      Other ~ "Other",
      No_impact ~ "No impact",
      TRUE ~ "No challenge"
    )
  ) %>%
  select(-Financial, -School, -Other, -No_impact)

tmp$challenge_cat %>% table()


challenge_num <- dem_family_subset %>%
  mutate(cope_pattern = apply(., 1, function(row) paste0(row, collapse = "")))


tmp1 <- tmp$cope_pattern %>% table() %>% data.frame()

recode_variables <- function(df) {
  df <- df %>% mutate(across(everything(), ~ as.integer(as.factor(.))))
  return(df)
}



family_dat_LCA <- dem_family_subset %>%
  recode_variables() %>%
  rename_with(~ str_remove(., "b_covid_family_"), starts_with("b_covid_family_"))

colnames(cope_dat_LCA) <- c( "connect", "healthworker", "alcohol",
                             "smoking")

f3 <- cbind(family_did_not_enough_enough_money_for_food, family_did_not_have_a_regular_place_to_sleep_or_stay,
        i_could_not_attend_school_in_person, i_could_not_attend_school_at_all,
        other, family_did_not_have_enough_money_for_gas_transportation,
        family_did_not_have_enough_money_to_pay_rent, the_covid_19_pandemic_has_not_affected_me_or_my_family_in_these_ways_in_the_past_2_weeks
) ~ 1

set.seed(101)
family_LCA3 <- poLCA(f3, data = family_dat_LCA,
                   nclass = 4)



probs_list <- family_LCA3$probs

#probs_list <- LCA4$probs


probs_df <- do.call(rbind, lapply(names(probs_list), function(var) {
  prob_df <- as.data.frame(probs_list[[var]])
  prob_df$variable <- var
  prob_df$class <- rownames(prob_df)
  prob_df
})) %>% dplyr::select(-`Pr(1)`)



probs_df_list <- list()


for (var in names(probs_list)) {
  prob_df <- as.data.frame(probs_list[[var]])
  prob_df$variable <- var
  prob_df$class <- rownames(prob_df)
  probs_df_list[[var]] <- prob_df
}


probs_df_combined <- bind_rows(probs_df_list)%>%
  dplyr::select(-`Pr(1)`)

ggplot(probs_df_combined, aes(x = variable, y =`Pr(2)` , color = class, group = class)) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Category", y = "Values", title = "Line Plot of Three Columns")


# 将长格式的数据框转换为宽格式
probs_df_wide <- probs_df_combined %>%
  pivot_wider(names_from = class, values_from = c( `Pr(2)`))


family_class <- family_LCA3$predclass

