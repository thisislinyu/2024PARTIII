load('data/dat_cc.rds')
library(dplyr)
library(caret)
library(sjPlot)
library(car)

## read dataset------
## read dataset and data preparation--

dat <- dat_cc %>% filter(condition!="Project Personality") ## ABC vs control

# dat <- dat_cc %>% filter(condition!="Project ABC") ## Personality vs control


set.seed(1017)

loocv_f <- function(data=NULL,  Y=NULL, cov = NULL, formula=NULL,...) {
  n <- nrow(data)
  errors <- numeric(n)



  formula <- as.formula(paste(Y,"~",
                                   paste(cov ,
                                         collapse = "+")))

  for (i in 1:n) {

    train_data <- data[-i, ]
    test_data <- data[i, , drop = FALSE]

    fit <- lm(formula, data = train_data)

    pred <- predict(fit, test_data)

    sse[i] <- (Y- pred)^2
  }

  mse <- mean(sse)
  return(mse)
}

baselines <- colnames(baselines_dat)

Y = dat$f1_cdi_mean

fit0_formula <- as.formula(paste("Y ~ ",
                                 paste(baselines ,
                                       collapse = "+")))



mse <- loocv_f(data = dat, Y = Y, forumla = fit0_formula)





trainIndex <- createDataPartition(dat$f1_cdi_mean, p = 0.8, list = FALSE)

trainingSet <- dat[trainIndex,]
testSet <- dat[-trainIndex,]

baselines_dat <- dat %>% select(-b_response_id,
                                  -condition,
                                  -b_cdi_mean,
                                  -f1_cdi_mean,
                                  -b_screener_age,
                                -family_num,
                                family_cat)




lp <- lm(fit0_formula, data=dat)
summary(lp)

predict(lp, dat)

## mse
mean((Y - predict(lp, dat))^2)





model1 <- lm(Y~b_cdi_mean+condition*lp,data = dat_cc)







summary(fit0)

summary(fit1)

# R-squared 和 Adjusted R-squared
cat("R-squared:", summary(model1)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model)$adj.r.squared, "\n")

# F-statistic 和 p 值
cat("F-statistic:", summary(model)$fstatistic[1], "\n")
cat("F-statistic p-value:", pf(summary(model)$fstatistic[1],
                               summary(model)$fstatistic[2],
                               summary(model)$fstatistic[3], lower.tail=FALSE), "\n")

# VIF 检查多重共线性
vif_values <- vif(model)
cat("VIF values:\n")
print(vif_values)

# 残差图
par(mfrow=c(2, 2))
plot(model)

# 标准化残差
library(MASS)
stdres <- studres(model)
hist(stdres, breaks=10, main="Standardized Residuals")

# 返回模型结果
model


set.seed(1017)
trainIndex <- createDataPartition(dat$f1_cdi_mean, p = 0.8, list = FALSE)

trainingSet <- dat[trainIndex,]
testSet <- dat[-trainIndex,]


######## linear regression

# colnames(trainingSet) <- c('f1_cdi_mean',paste0('var',c(1:(ncol(trainingSet)-1 ) )))
# colnames(testSet) <- c('f1_cdi_mean',paste0('var',c(1:(ncol(trainingSet)-1 ) )))

##modelling----

### set up-----
rfFuncs$summary <- BigSummary

objectives = 'auc_ap_sbr'  #'auc_ap_sbr',"SBrier"

twoClassCtrl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 10,
  savePredictions = "final",
  classProbs = T,
  summaryFunction = BigSummary
)
### repeated cv training---
set.seed(20230517)
glmnet_m <- train(f1_cdi_mean ~., data = trainingSet,
                  method = "glmnet",   #glmnet
                  metric = objectives,
                  trControl = twoClassCtrl,
                  tuneLength = 10
)


get_best_result(glmnet_m)
glmnet_m$bestTune


tune_values <- table(glmnet_m$results$alpha,glmnet_m$results$lambda) %>% data.frame()

## coef for non-IRP
coef_glmnet <- coef(glmnet_m$finalModel, glmnet_m$finalModel$lambdaOpt) %>%
  matrix() %>% data.frame()
f1_cdi_mean_n <- dat %>% colnames() %>% length()
rownames(coef_glmnet) <- c("intercept",colnames(dat)[-f1_cdi_mean_n])
coef_glmnet <-  coef_glmnet %>% filter(.!=0) %>%
  mutate(.=.*-1)
# equivalent to coef_glmnet <- coef(glmnet_m$finalModel, glmnet_m0517111$finalModel$lambdaOpt)
# test <- glmnet(x = trainingSet[,-1] ,y=trainingSet[,1],
#                family = 'binomial',
#                alpha =  1, # glmnet_m$bestTune
#                lambda =0.0260704)

# coef_table <- coef(test) %>% matrix() %>% data.frame()
# rownames(coef_table) <- c("intercept",colnames(model_dat_82X_1)[-f1_cdi_mean_n])
# coef_table <- coef_table %>% filter(.!=0)

### evaluate model performance------
set.seed(20230517)
roc_f(model_m = glmnet_m,testdat= testSet)

glm_perf <- sum_perf_f(glmnet_m, testdat=testSet,model_name='ElasticNet') %>% data.frame()

par(mfrow=c(1,2))
cali_plot_f(model_m=glmnet_m,model_name='',testdat=testSet)

ggsave("cali0518.pdf")
dev.off()

ks_glm <- kernelshap(glmnet_m, trainingSet[,-1],
                     bg_X = trainingSet,
                     type="prob")


ks_glm1 <- ks_glm$S[[1]]

ks_glm2 <- ks_glm$S[[2]]


ks_glm1_abs <- apply(ks_glm11,2,abs) %>% data.frame()


ks_feature_importance <- apply(ks_glm1_abs,2,mean) %>% data.frame()

ks_feature_importance$. %>% sum()


ks_feature_importance1 <- ks_feature_importance %>%
  mutate(fi = ./sum(.),
         variable_name = rownames(ks_feature_importance))

sv_importance(shapviz(ks_glm1,trainingSet[,-1]), "bar",
              fill = "#8f8f8f", show_other = FALSE,
              max_display = nrow(coef_glmnet)-1)

sv_importance(shapviz(ks_glm1,trainingSet[,-1]), "bee",
              color_bar_title = 'Feature Value',
              viridis_args = list(begin = 0.13, end = 1, option = "turbo"),
              max_display = nrow(coef_glmnet)-1)

sv_importance(shapviz(ks_glm1,trainingSet[,-1]), "bee")


sv_importance(shapviz(ks_glm2,trainingSet[,-1]), "bee")


glm_cat_val <- pred_cat_f(model_m = glmnet_m,model_name='ElasticNet',testdat=testSet,data_type = 'Validation')%>% data.frame()

glm_cat_test <- pred_cat_f(model_m = glmnet_m,model_name='ElasticNet',
                           testdat=testSet,data_type = 'Test')%>% data.frame()

cat_sum <- rbind(glm_cat_test,
                 glm_cat_val
)

DT::datatable(cat_sum,
              filter = "top", editable = "cell", extensions = "Buttons",
              options = list(
                dom = "Blfrtip",
                scrollX = TRUE,
                buttons = c("copy", "csv", "excel", "pdf", "print"),
                lengthMenu = list(
                  c(5, 25, 50, 100, -1),
                  c(5, 25, 50, 100, "All")
                )
              )
)




























