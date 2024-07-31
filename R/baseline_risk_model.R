load('data/dat_cc.rds')
library(dplyr)
library(caret)
library(sjPlot)
library(car)

## read dataset------
## read dataset and data preparation--

rmse <- NULL
internal_lp <- NULL
for(i in c(1:nrow(dat))){

  trainset = dat[-i, ]
  testset = dat[i,]

baselines_dat <- trainset %>% dplyr::select(-b_response_id,
                                -condition,
                                -b_cdi_mean,
                                -f1_cdi_mean,
                                -b_screener_age,
                                -family_num,
                                family_cat)

baselines <- colnames(baselines_dat)

Y = trainset$f1_cdi_mean

fit0_formula <- as.formula(paste("Y ~ ",
                                 paste(baselines ,
                                       collapse = "+")))

lp_fit <- lm(fit0_formula, data=trainset)

## mse
mse[i] <-sqrt((testset$f1_cdi_mean - predict(lp_fit, testset))^2)

internal_lp[i] <- predict(lp_fit, testset)


}

# Calculate Calibration Slope
calibration_model <- lm(dat$f1_cdi_mean ~ internal_lp)
calibration_slope <- coef(calibration_model)[2]

# Calculate Calibration in the Large
calibration_intercept <- coef(calibration_model)[1]


plot(internal_lp,dat$f1_cdi_mean)





## final model
baselines_dat <- dat %>% dplyr::select(-b_response_id,
                                     -condition,
                                     -b_cdi_mean,
                                     -f1_cdi_mean,
                                     -b_screener_age,
                                     -family_num,
                                     family_cat)

baselines <- colnames(baselines_dat)

Y = dat$f1_cdi_mean

fit_final_formula <- as.formula(paste("Y ~ ",
                                 paste(baselines ,
                                       collapse = "+")))

lp_final <- lm(fit_final_formula, data=dat)








lp_baseline_risk <- predict(lp_final, dat)


heter_dat <- dat %>%
  mutate(lp_baseline_risk = lp_baseline_risk)



plot(heter_dat$f1_cdi_mean,predict(fit1,heter_dat))














### heterogeneity model
fit1_formula <- as.formula("Y~ b_cdi_mean+condition*lp_baseline_risk")

fit00_formula <- as.formula("Y~ b_cdi_mean+condition")

fit1 <- lm(fit1_formula, data=heter_dat)
summary(fit1)

(heter_dat$f1_cdi_mean- predict(fit1,heter_dat))^2 %>% mean()


fit00 <- lm(fit00_formula, data=heter_dat)

summary(fit00)

(heter_dat$f1_cdi_mean- predict(fit00,heter_dat))^2 %>% mean()


# cohen.d(dat$f1_cdi_mean[dat$condition=="Placebo Control"],
#         dat$f1_cdi_mean[dat$condition=="Project ABC"])
#



hete_pred <- predict(fit1,heter_dat)

main_pred <- predict(fit00,heter_dat)




heter_dat_control <- heter_dat

heter_dat_control$condition = 'Placebo Control'

heter_dat_trt <- heter_dat

heter_dat_trt$condition = 'Project Personality'

control_pred <- predict(fit1,heter_dat_control)

trt_pred <- predict(fit1,heter_dat_trt)




heter_mean_diff <- trt_pred -control_pred

################# in the main effect model
heter_dat00_control <- heter_dat

heter_dat00_control$condition = 'Placebo Control'

heter_dat00_trt <- heter_dat

heter_dat00_trt$condition = 'Project Personality'

control00_pred <- predict(fit00,heter_dat00_control)

trt00_pred <- predict(fit00,heter_dat00_trt)




heter_mean_diff00 <- trt00_pred -control00_pred


cal_dat <- data.frame(heter_mean_diff,heter_mean_diff00,lp_baseline_risk)


plot(heter_mean_diff*12)



plot(lp_baseline_risk,heter_mean_diff)

lp_baseline_risk %>% summary()

calibration.plot()


histogram(lp_baseline_risk)


cal_dat$quantile_group <- cut(cal_dat$lp_baseline_risk,
                           breaks = quantile(cal_dat$lp_baseline_risk, probs = seq(0, 1, by = 0.2)),
                           include.lowest = TRUE,
                           labels = FALSE)
group_means <- cal_dat %>%
  group_by(quantile_group) %>%
  summarise(mean_heter_mean_diff = mean(heter_mean_diff, na.rm = TRUE))


set.seed(1017)
boot_res <- NULL
for (i in 1:1000){
  boot.idx <- sample(1:dim(cal_dat)[1], size = dim(cal_dat)[1], replace = T)
  boot.data <- cal_dat[boot.idx,]

  boot.data$quantile_group <- cut(boot.data$lp_baseline_risk,
                                breaks = quantile(boot.data$lp_baseline_risk, probs = seq(0, 1, by = 0.2)),
                                include.lowest = TRUE,
                                labels = FALSE)
  group_means <- boot.data %>%
    group_by(quantile_group) %>%
    summarise(mean_heter_mean_diff = mean(heter_mean_diff, na.rm = TRUE))
 boot_res <- rbind(boot_res,group_means)
}

boot_cali_ci <- boot_res %>%
  group_by(quantile_group) %>%
  summarise(
    lower_quantile = round(quantile(mean_heter_mean_diff, probs = 0.025), 4),
    upper_quantile = round(quantile(mean_heter_mean_diff, probs = 0.975), 4)
  )


cali_result <- left_join(group_means,boot_cali_ci)


ggplot(cali_result, aes(x = as.factor(quantile_group), y = mean_heter_mean_diff)) +
  geom_point(size = 2.5) +
  # geom_line(aes(group = 1), color = "blue") +
  geom_errorbar(aes(ymin = lower_quantile, ymax = upper_quantile), width = 0.05, color = "black") +
  labs(x = "Quantile Group of lp_baseline_risk",
       y = "Mean heter_mean_diff",
       title = "Calibration Plot with Bootstrap CI") +
  geom_hline(yintercept = cal_dat$heter_mean_diff00, linetype = "dashed", color = "grey")+
  theme_minimal()+
  ylab(expression(atop("Risk Difference, %", atop("Harm" %<-% phantom("     ") %->% "Benefit")))) +  # Custom y-axis label
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, size = 14))


#"95% confidence interval for ITT effect estimate:"







print(group_means)


plot(group_means$quantile_group, group_means$mean_heter_mean_diff,
     type = "b", pch = 19, col = "blue",
     xlab = "Quantile Group of lp_baseline_risk",
     ylab = "Mean heter_mean_diff",
     main = "Calibration Plot")

# 添加平滑曲线
lines(smooth.spline(group_means$quantile_group, group_means$mean_heter_mean_diff), col = "red")













