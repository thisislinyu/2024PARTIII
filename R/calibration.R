calibration_plot <- ggplot(cali_result, aes(x = as.factor(quantile_group), y = mean_heter_mean_diff)) +
  geom_point(size = 2.5) +
  # geom_line(aes(group = 1), color = "blue") +
  geom_errorbar(aes(ymin = lower_quantile, ymax = upper_quantile), width = 0.05, color = "black") +
  labs(x = "Quantile Group of lp_baseline_risk",
       y = "Mean heter_mean_diff",
       # title = ""
       ) +
  geom_hline(yintercept = cal_dat$heter_mean_diff00, linetype = "dashed", color = "grey")+
  theme_minimal()+
  ylab("Treatment difference")+
  xlab("")+
 # ylab(expression(atop("Risk Difference, %", atop("Benefit" %<-% phantom("     ") %->% "Benefit")))) +  # Custom y-axis label
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, size = 14))



# Distribution plot
distribution_plot <- ggplot(cal_dat, aes(x = lp_baseline_risk)) +
  geom_histogram(binwidth = 0.005, fill = "black") +
  labs(x = "Predicted CDI mean score using baseline covariates", y = NULL) +
  theme_minimal()

# Combine the plots
combined_plot <- plot_grid(
  calibration_plot,
  distribution_plot,
  ncol = 1,
  align = "v",
  rel_heights = c(3, 1)  # Adjust relative heights if needed
)

# Display the combined plot
print(combined_plot)

### use 10 cutoff



