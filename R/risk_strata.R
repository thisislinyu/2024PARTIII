
cali10_plot_dat

cali10_plot_dat$quantile_grp = factor(cali10_plot_dat$quantile_grp,levels =c("1","2","3","4","5","6","7","8","9","10") )
extended_levels <-c(rev(levels(cali10_plot_dat$quantile_grp)),11)
new_labels <- c(as.character(c(10:1)),"")

cali10_plot <- ggplot(cali10_plot_dat, aes(x = as.factor(quantile_grp), y = abc_avg_hte)) +
  geom_point(size = 2.5,shape=15,color = "#1f78b4") +
  geom_errorbar(aes(ymin = lower_quantile, ymax = upper_quantile), width = 0.15,color = "#1f78b4") +
  labs(x = "", y = "Mean difference of 3-month CDI-SF mean score(Project ABC vs Placebo Control)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = abc_ate, linetype = "dashed",color = "#1f78b4") +
  theme_minimal() +
  scale_y_continuous(limits = c(-0.2, 0.1), breaks = seq(-0.2, 0.1, by = 0.05)) +
  coord_flip() +
  #scale_x_discrete(limits = rev(levels(cali10_plot_dat$quantile_grp)))+
  scale_x_discrete(limits = extended_levels,labels = new_labels) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, size = 12))+
  theme(axis.text.x = element_text(size = 12))
cali10_plot



#### table plot
wide10plot <- wide_10group_plot %>% mutate(`Placebo Control` = as.character(`Placebo Control`),
                                           `Project ABC` = as.character(`Project ABC`),
                                           `Num.~Control/ABC^1`= paste0(`Placebo Control`,"/",`Project ABC`)
                                           #`Score~interval^1` = `Score interval`

)

wide10plot <- rbind(colnames(wide10plot),wide10plot)

wide10plot1 <- wide10plot
wide10plot1$`Score group` <- factor(wide10plot$`Score group`,
                                    levels = c("Score group", "1 (Lowest)", "2", "3", "4", "5", "6", "7",
                                               "8", "9", "10 (Highest)"))

wide10plot1$`Mean difference`[wide10plot1$`Mean difference`=="-0.07(-0.12,0)"] = "-0.07(-0.12,0.00)"
wide10plot1$`Mean difference`[wide10plot1$`Mean difference`=="-0.1(-0.16,-0.04)"] = "-0.10(-0.16,-0.04)"
wide10plot1$`Mean difference`[wide10plot1$`Mean difference`=="-0.11(-0.2,-0.04)"] = "-0.11(-0.20,-0.04)"

# wide10plot1$`Score group`[wide10plot1$`Score group`=="Score group"] = "Score~group^1"
# wide10plot1$`Score interval`[wide10plot1$`Score interval`=="Score interval"] = "Score~interval^2"
# wide10plot1$`Mean difference`[wide10plot1$`Mean difference`=="Mean difference"] = "Mean~difference^4"



wide10plot1$myscale <- c(1.2,2.1,3.1,4.1,5,6,7,8,9,10,11)
wide10plot1$myscale <- c(1.1, c(2:11)+0.05)



table_text <- ggplot(wide10plot1, aes(x = 1, y = myscale)) +
  geom_text(aes(label = `Score group`), size = 3.5, hjust = 0,nudge_x = 0) +
  geom_text(aes(label =`Score interval` ), size = 3.5,  hjust = 0.5, vjust = 0.5, nudge_x = 1.2) +
  #geom_text(aes(label = `Placebo Control`), size = 3.5, hjust = 0, nudge_x = 1.8) +
  #geom_text(aes(label = `Project ABC`), size = 3.5, hjust = 0, nudge_x = 3) +
  geom_text(aes(label = `Num.~Control/ABC^1`), size = 3.5, hjust = 0.5, vjust = 0.5, nudge_x = 2.3,parse = TRUE)+
  geom_text(aes(label = `Mean difference`), size = 3.5,  hjust = 0.5, vjust = 0.5, nudge_x = 3.5) +
  theme_void() +
  xlim(1, 5) +
  ylim(12, 1)


combined_plot <- plot_grid(table_text, cali10_plot, ncol = 2, rel_widths = c(2, 3))


print(combined_plot)

ggsave("figures/risk_strata.png", combined_plot, width = 12, height = 6, dpi = 600)



