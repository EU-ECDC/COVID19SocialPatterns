estBeta_mcaNPI <- function(country,percentile) {

df <- cbind(fitted_beta_coefficients(country,percentile),
            (mca_factors(country) %>%
               filter(`NPI$date`>=min(comix_dates(country)[,1]))%>%
               filter(`NPI$date`<=max(comix_dates(country)[,1])))[,-1])

long_data <- pivot_longer(df, cols = -Date, names_to = "variable", values_to = "value")

subset_top <- long_data %>% filter(variable %in% c("Date", "beta1", "beta2", "beta3", "beta4",
                                                   "beta1_w", "beta2_w", "beta3_w", "beta4_w"))
# Create the ggplot for the top plot
plot_top <- ggplot(subset_top, aes(x = Date, y = value, color = variable)) +
  geom_line(data = subset(subset_top, variable %in% c("beta1", "beta2", "beta3", "beta4")), show.legend = TRUE) +
  geom_point(data = subset(subset_top, variable %in% c("beta1_w", "beta2_w", "beta3_w", "beta4_w")), show.legend = TRUE) +
  labs(x = "Time", y = "Median estimated change in contacts per setting", title = "Dependent") +
  scale_color_discrete(name = "Time-varying Coefficients", 
                       breaks = c("beta1", "beta2", "beta3", "beta4"))+
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

# Subset the data for the second ggplot (bottom plot)
subset_bottom <- long_data %>% filter(variable %in% c("Date", "Dim 1", "Dim 2", "Dim 3", "Dim 4","Dim 5"))

# Create the ggplot for the bottom plot
plot_bottom <- ggplot(subset_bottom, aes(x = Date, y = value, color = variable)) +
  geom_line() +
  labs(x = "time", y = "MCA factors", title = "Independent") +
  scale_color_discrete(name = "Factors")+
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot_top + plot_bottom + plot_layout(ncol = 1)

return(combined_plot)
}
