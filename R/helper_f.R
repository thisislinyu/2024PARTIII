boot1_f <- function(mydat=NULL,seed=1017){
  set.seed(seed)
  abc_boot_res <- NULL
  for (i in 1:1000){
    boot.idx <- sample(1:dim(mydat)[1], size = dim(mydat)[1], replace = T)
    boot.data <- mydat[boot.idx,]

    abc_main_dat <- boot.data %>%  dplyr::select(f1_cdi_mean,condition,b_cdi_mean)

    abc_main_m <- lm(f1_cdi_mean~ b_cdi_mean + condition,data=abc_main_dat)

    lp_abc <- predict(abc_main_m,boot.data)

    abc_ate <- coef(abc_main_m)[3]

    abc_hte_m <- lm(f1_cdi_mean~ b_cdi_mean + condition*lp_abc,data=boot.data)

    ## cATE/HTE
    ## project ABC vs control
    hte_dat <- boot.data

    trt_levels <- unique(boot.data$condition) %>% as.character()

    trt <- trt_levels[!(trt_levels %in% 'Placebo Control')]

    hte_dat$condition = 'Placebo Control'
    abc_hte_ctrl <- predict(abc_hte_m,hte_dat)

    hte_dat$condition  = trt
    abc_hte_trt <- predict(abc_hte_m,hte_dat)

    abc_hte <- abc_hte_trt-abc_hte_ctrl



    ## calibration plot
    abc_cal_dat <- data.frame(abc_ate,
                              abc_hte,
                              lp_abc)

    abc_cal_dat$quantile_grp <- cut(abc_cal_dat$lp_abc,
                                    breaks = quantile(abc_cal_dat$lp_abc, probs = seq(0, 1, by = 0.1)),
                                    include.lowest = TRUE,
                                    labels = FALSE)

    ## compute the averaged hte effect in each quantile risk grp
    avg_hte <- abc_cal_dat %>%
      group_by(quantile_grp) %>%
      summarise(avg_hte = mean(abc_hte, na.rm = TRUE))

    abc_boot_res <- rbind(abc_boot_res,avg_hte)
  }
  return(abc_boot_res)
}
