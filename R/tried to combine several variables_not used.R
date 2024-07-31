combined_LCA_dat <- cbind(genderid_dat_LCA,cope_dat_LCA)






combined_LCA_dat <- cbind(genderid_dat_LCA,dem_race_subset)

f4 <- cbind(
  # b_dem_sex,
  agender, not_sure, other_please_specify,
  androgynous, nonbinary, two_spirited,
  female_to_male_transgender_ftm, trans_female_trans_feminine,
  trans_male_trans_masculine,
  # gender_expansive,
  third_gender, genderqueer, male_to_female_transgender_mtf,
  man_boy,
  # transgender,
  woman_girl,
  connecting_with_others, contacting_a_healthcare_provider,
  drinking_alcohol, smoking_more_cigarettes_or_vaping_more) ~ 1


combined_LCA2 <- poLCA(f4, data = combined_LCA_dat,
              nclass = 3)


f5 <- cbind(
  # b_dem_sex,
  agender, not_sure, other_please_specify,
  androgynous, nonbinary, two_spirited,
  female_to_male_transgender_ftm, trans_female_trans_feminine,
  trans_male_trans_masculine,
  # gender_expansive,
  third_gender, genderqueer, male_to_female_transgender_mtf,
  man_boy,
  # transgender,
  woman_girl,
  connecting_with_others, contacting_a_healthcare_provider,
  drinking_alcohol, smoking_more_cigarettes_or_vaping_more) ~ 1


combined_LCA2 <- poLCA(f4, data = combined_LCA_dat,
                       nclass = 3)

probs_list <- combined_LCA2$probs

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

probs_df_combined$variable <- factor(probs_df_combined$variable,
                                     levels = c("agender",
                                                "not_sure",
                                                "other_please_specify",
                                                "androgynous",
                                                "nonbinary",
                                                "two_spirited",
                                                "female_to_male_transgender_ftm",
                                                "trans_female_trans_feminine",
                                                "trans_male_trans_masculine",
                                                "third_gender",
                                                "genderqueer",
                                                "male_to_female_transgender_mtf",
                                                "man_boy",
                                                "woman_girl",
                                                "connecting_with_others",
                                                "contacting_a_healthcare_provider",
                                                "drinking_alcohol",
                                                "smoking_more_cigarettes_or_vaping_more"))


ggplot(probs_df_combined, aes(x = variable, y =`Pr(2)` , color = class, group = class)) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Category", y = "Values", title = "Line Plot of Three Columns")





