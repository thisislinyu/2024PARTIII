
library(dplyr)
library(readr)
library(stringr)
library(poLCA)
library(ggplot2)

cope_subset <- read_csv("data/cope_subset.csv")
dem_cope_subset <- cope_subset %>% dplyr::select(starts_with("b_covid_cope"))

## check correlation
# two variables are highly correlated, keep one
# b_covid_cope_1_connecting_with_others
# b_covid_cope_1_including_talking_with_people_you_trust_about_your_concerns_and_how_you_are_feeling

cope_cor <- cor(dem_cope_subset) %>% data.frame()

dem_cope_subset <- dem_cope_subset %>%
  dplyr::select(-"b_covid_cope_1_including_talking_with_people_you_trust_about_your_concerns_and_how_you_are_feeling")


tmp <- dem_cope_subset%>%
  mutate(cope_pattern = apply(., 1, function(row) paste0(row, collapse = "")))


tmp1 <- tmp$cope_pattern %>% table() %>% data.frame()

recode_variables <- function(df) {
  df <- df %>% mutate(across(everything(), ~ as.integer(as.factor(.))))
  return(df)
}

cope_dat_LCA <- dem_cope_subset %>%
  recode_variables() %>%
  rename_with(~ str_remove(., "b_covid_cope_1_"), starts_with("b_covid_cope_1_"))

colnames(cope_dat_LCA) <- c( "connect", "healthworker", "alcohol",
                             "smoking")

f2 <- cbind(connecting_with_others, contacting_a_healthcare_provider,
        drinking_alcohol, smoking_more_cigarettes_or_vaping_more) ~ 1

set.seed(1017)
cope_LCA3 <- poLCA(f2, data = cope_dat_LCA,
              nclass = 3)


probs_list <- cope_LCA3$probs

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



probs_df_wide <- probs_df_combined %>%
  pivot_wider(names_from = class, values_from = c( `Pr(2)`))




tmp <- cbind(cope_dat_LCA,cope_LCA3$posterior)



cope_class <- cope_LCA3$predclass


