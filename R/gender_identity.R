#poLCA
library(poLCA)
library(dplyr)
library(readr)
library(stringr)
library(poLCA)
library(ggplot2)
library(reshape2)
library(RColorBrewer)


cope_subset <- read_csv("data/cope_subset.csv")
dem_gender_subset <- cope_subset %>% dplyr::select(starts_with("b_dem_gender"))


tmp <- dem_gender_subset%>%
  mutate(cope_pattern = apply(., 1, function(row) paste0(row, collapse = "")))

## 16 combinations
tmp1 <- tmp$cope_pattern %>% table() %>% data.frame()
# trans_num <- cope_subset %>%  dplyr::select(
#   #b_response_id,
#   #  b_dem_sex,
#   b_dem_gender_transgender,
#   b_dem_gender_female_to_male_transgender_ftm,
#   b_dem_gender_male_to_female_transgender_mtf,
#   b_dem_gender_trans_female_trans_feminine,
#   b_dem_gender_trans_male_trans_masculine) %>%apply(.,1,sum)
#
# genderid_dat  <-  cope_subset %>%  dplyr::select(
#   b_response_id,
#    b_dem_sex,
#   b_dem_gender_transgender,
#  ) %>%
#   mutate(trans_num = trans_num)%>%
#   cbind(cope_subset %>%  dplyr::select(
#     #b_response_id,
#     #  b_dem_sex,
#     # b_dem_gender_transgender,
#     b_dem_gender_female_to_male_transgender_ftm,
#     b_dem_gender_male_to_female_transgender_mtf,
#     b_dem_gender_trans_female_trans_feminine,
#     b_dem_gender_trans_male_trans_masculine) ) %>%
#   select(-trans_num)

### if any data entry error

## if transgender ==1, the subgroup should be greater than 1
#
# (genderid_dat$trans_num[(genderid_dat$b_dem_gender_transgender==1)]>0) %>% table()
#
# ## if transgender ==0, the subgroup should be 0
#
# (genderid_dat$trans_num[(genderid_dat$b_dem_gender_transgender==0)]<=0) %>% table()
#
# ##
# entry_error <- genderid_dat %>% filter(b_dem_gender_transgender==0 & trans_num>0)
#
# tmp <- genderid_dat %>% filter(b_dem_gender_transgender==1)



genderid_dat <- cope_subset %>%
  dplyr::select(b_response_id,c(b_dem_sex, b_dem_gender_agender, b_dem_gender_not_sure, b_dem_gender_other_please_specify,
                                                                     b_dem_gender_androgynous, b_dem_gender_nonbinary, b_dem_gender_two_spirited,
                                                                     b_dem_gender_female_to_male_transgender_ftm, b_dem_gender_trans_female_trans_feminine,
                                                                     b_dem_gender_trans_male_trans_masculine,
                                                                     b_dem_gender_gender_expansive,
                                                                     b_dem_gender_third_gender, b_dem_gender_genderqueer, b_dem_gender_male_to_female_transgender_mtf,
                                                                     b_dem_gender_man_boy,
                                                                     b_dem_gender_transgender,
                                                                     b_dem_gender_woman_girl
))



table1_trans <- tbl_summary(genderid_dat %>% dplyr::select(-b_response_id),
                     # by = condition,
                      statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                       all_categorical() ~ "{n} ({p}%)"), digits = all_continuous() ~ 2,
                      label = list(
                        # b_dem_race_american_indian_or_alaska_native ~ "American Indian or Alaska Native",
                                 #  b_dem_race_asian_including_asian_desi ~ "Asian Including Asian Desi",
                                #   b_dem_race_hispanic_latinx ~ "Hispanic/Latinx",
                                #   b_dem_race_native_hawaiian_or_other_pacific_islander ~ "Native Hawaiian or Other Pacific Islander",
                                 #  b_dem_race_white_caucasian_non_hispanic_includes_middle_eastern ~ "White",
                                 #  b_dem_race_black_african_american ~"Black/African-American",
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
                                   b_dem_sex ~ "Biological Sex"
                                   #b_dem_orientation ~ "Sexual Orientation"
                                   #,
                                   # b_cdi_sum ~ "Baseline CDI Sum Score (0-24)"
                      )
)





genderid_dat_cor <- cor(genderid_dat) %>% data.frame()

c(b_dem_sex, b_dem_gender_agender, b_dem_gender_not_sure, b_dem_gender_other_please_specify,
  b_dem_gender_androgynous, b_dem_gender_nonbinary, b_dem_gender_two_spirited,
  b_dem_gender_female_to_male_transgender_ftm, b_dem_gender_trans_female_trans_feminine,
  b_dem_gender_trans_male_trans_masculine,
  b_dem_gender_gender_expansive,
  b_dem_gender_third_gender, b_dem_gender_genderqueer, b_dem_gender_male_to_female_transgender_mtf,
  b_dem_gender_man_boy,
 b_dem_gender_transgender,
  b_dem_gender_woman_girl
)




genderid_dat_cor_melted <- melt(as.matrix(genderid_dat_cor))


ggplot(data = genderid_dat_cor_melted, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(x = "", y = "") +
  ggtitle("Correlation Matrix Heatmap")



recode_variables <- function(df) {
  df <- df %>% mutate(across(everything(), ~ as.integer(as.factor(.))))
  return(df)
}


genderid_dat_LCA <- genderid_dat %>%
  dplyr::select(-b_response_id) %>%
  recode_variables() %>%
  rename_with(~ str_remove(., "b_dem_gender_"), starts_with("b_dem_gender_"))


head(genderid_dat)


f1 <- cbind(
  # b_dem_sex,
  agender, not_sure, other_please_specify,
  androgynous, nonbinary, two_spirited,
  female_to_male_transgender_ftm, trans_female_trans_feminine,
  trans_male_trans_masculine,
  # gender_expansive,
  third_gender, genderqueer, male_to_female_transgender_mtf,
  man_boy,
  # transgender,
  woman_girl) ~ 1



set.seed(1017)
LCA2 <- poLCA(f1, data = genderid_dat_LCA,
              nclass = 2)

print(probs_df_wide)

LCA2$posterior

gender_LCA3 <- poLCA(f1, data = genderid_dat_LCA,
              nclass = 3)

LCA4 <- poLCA(f1, data = genderid_dat_LCA,
              nclass = 4)


LCA5 <- poLCA(f1, data = genderid_dat_LCA,
              nclass = 5)



print(gender_LCA3)



genderid_class <- gender_LCA3$predclass




print(genderid_dat)


# probabilities of answering yes by class
probs_list <- gender_LCA3$probs

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



# probs_df_wide <- probs_df_combined %>%
#   pivot_wider(names_from = class, values_from = c( `Pr(2)`))
#
# heatmap_dat <- probs_df_wide %>% dplyr::select(-variable) %>% as.matrix()
#
# heatmap(heatmap_dat)


gender_post <- gender_LCA3$posterior %>% data.frame()

tmp <- apply(gender_post,1,sum) %>% data.frame()

