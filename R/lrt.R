
full_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + recode_orient + genderid3 + family_cat + cope3

gender_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + recode_orient + family_cat + cope3

race_formula <- f1_cdi_mean ~  b_dem_sex + recode_orient + genderid3 + family_cat + cope3

sex_formula <- f1_cdi_mean ~ recode_race + recode_orient + genderid3 + family_cat + cope3


orient_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + genderid3 + family_cat + cope3

family_cat_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + recode_orient + genderid3  + cope3

cope_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + recode_orient + genderid3 + family_cat



full_person <- lm(as.formula(full_formula),data =basemodel_dat_person )
gender_person <- lm(as.formula(gender_formula),data = basemodel_dat_person)
lrt_result <- lmtest::lrtest(gender_person,full_person)

deviance <- lrt_result$Chisq[2]

# Extract the degrees of freedom
df <- lrt_result$Df[2]

# Extract the p-value
p_value <- lrt_result$`Pr(>Chisq)`[2] %>% round(2)




full_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + recode_orient + genderid3 + family_cat + cope3
gender_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + recode_orient + family_cat + cope3
race_formula <- f1_cdi_mean ~ b_dem_sex + recode_orient + genderid3 + family_cat + cope3
sex_formula <- f1_cdi_mean ~ recode_race + recode_orient + genderid3 + family_cat + cope3
orient_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + genderid3 + family_cat + cope3
family_cat_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + recode_orient + genderid3 + cope3
cope_formula <- f1_cdi_mean ~ recode_race + b_dem_sex + recode_orient + genderid3 + family_cat


full_abc <- lm(as.formula(full_formula), data = basemodel_dat_abc)
full_person <- lm(as.formula(full_formula), data = basemodel_dat_person)

lrt_person_f <- function(full_model, reduced_formula) {
  reduced_model <- lm(as.formula(reduced_formula), data = basemodel_dat_person)
  lrt_result <- lrtest(reduced_model,full_model )

  deviance <- lrt_result$Chisq[2]
  df <- lrt_result$Df[2]
  p_value <- round(lrt_result$`Pr(>Chisq)`[2], 2)

  return(data.frame(Deviance = deviance, DF = df, P_value = p_value))
}

lrt_abc_f <- function(full_model, reduced_formula) {
  reduced_model <- lm(as.formula(reduced_formula), data = basemodel_dat_abc)
  lrt_result <- lrtest(reduced_model,full_model)

  deviance <- lrt_result$Chisq[2]
  df <- lrt_result$Df[2]
  p_value <- round(lrt_result$`Pr(>Chisq)`[2], 2)

  return(data.frame(Deviance = deviance, DF = df, P_value = p_value))
}


reduced_models <- list(
  `Gender identity` = gender_formula,
  `Race` = race_formula,
  `Biological sex` = sex_formula,
  `Sexual Orientation` = orient_formula,
  `Challenges` = family_cat_formula,
  `Coping strategies` = cope_formula
)

## lrt for project ABC
dev_results_abc <- lapply(names(reduced_models), function(var) {
  result <- lrt_abc_f(full_abc, reduced_models[[var]])
  result$Variable <- var
  return(result)
})


Dev_df_abc <- bind_rows(dev_results_abc) %>%
  dplyr::select(Variable,everything())

kable(Dev_df_abc)

## perform lrt (person)
dev_results_person <- lapply(names(reduced_models), function(var) {
  result <- lrt_person_f(full_person, reduced_models[[var]])
  result$Variable <- var
  return(result)
})




Dev_df_person <- bind_rows(dev_results_person) %>%
  dplyr::select(Variable,everything())

kable(Dev_df_person )
