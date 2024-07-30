# source('R/baseline_risk_model.R')

load('data/dat_cc.rds')
library(dplyr)
library(caret)
library(sjPlot)
library(car)

## read dataset------
## read dataset and data preparation--

dat <- dat_cc %>% filter(condition!="Project Personality") ## ABC vs control


baselines_dat <- train %>% select(-b_response_id,
                                  -condition,
                                  -b_cdi_mean,
                                  -f1_cdi_mean,
                                  -b_screener_age,
                                  -family_num,
                                  family_cat)

baselines <- colnames(baselines_dat)

Y = train$f1_cdi_mean

fit0_formula <- as.formula(paste("Y ~ ",
                                 paste(baselines ,
                                       collapse = "+")))



lp <- lm(fit0_formula, data=train)

fit1_formula <- as.formula("Y~ b_cdi_mean+condition*lp")



  lm(, data=dat_cc)




