library(tableone)
library(WeightIt)
library(tidyverse)
library(cobalt)
library(geepack)
library(xgboost)
library(tmle)
library(DiagrammeR)
library(cobalt)
library(survey)
library(gfoRmula)
library(ltmle)
library(rsvg)
library(xtable)

canpath_subset2 <- read_excel("data/canpath_subset2.xlsx")

covariates <- select(canpath_subset2, -c(id, HRT))
baselines <- colnames(covariates)
baselines
tab0 <- CreateTableOne(vars = baselines,
                       data = canpath_subset2,
                       strata = "HRT",
                       test = FALSE, #mute P-value calculation;
                       smd = TRUE,
                       addOverall = TRUE)


table1_out <- as.data.frame(print(tab0, smd = TRUE, showAllLevels = FALSE))

# Convert dataframe to xtable
latex_table <- xtable(table1_out)



writeLines(print(latex_table, type = "latex"), "tableone_output122111.tex")
