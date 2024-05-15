pred_CM_noCoMix <- function(country_1, country_2){
  
loaded_fit <- load_reg_fit(country_1, "50")
posts_1 <- rstan::extract(loaded_fit)

iterations <- 12000
fit_delta0 <- posts_1$delta0
fit_delta <- posts_1$delta

fit_L_Sigma <- posts_1$L_Sigma
cov_matrix <- array(NA, dim = c(iterations, 4, 4))
for (i in 1:iterations) {
  cov_matrix[i,,] <- fit_L_Sigma[i,,] %*% t(fit_L_Sigma[i,,])
}

x_pred <- (mca_factors(country_2))[,-1]
x_pred <- as.matrix(x_pred)
mu <- array(NA, dim = c(iterations, 4, nrow(x_pred)))
beta <- array(NA, dim = c(iterations, 4, nrow(x_pred)))
for (i in 1:iterations) {
  for (t in 1:nrow(x_pred)) {
    mu[i, , t] <- fit_delta0[i, ] + fit_delta[i, , ] %*% x_pred[t, ]
    beta[i, , t] <- MASS::mvrnorm(1, mu = mu[i, , t], Sigma = cov_matrix[i,,])
  }
}

tvem_fit <- load_fit(country_1) 
posts_2 <- rstan::extract(tvem_fit)
fit_beta0 <- posts_2$beta0

X <- t(as.matrix(contact_data(country_2)[[1]][,]))
C_pred <- array(NA, dim = c(iterations, nrow(x_pred), 9))
for (i in 1:iterations) {
  for (t in 1:nrow(x_pred)){
    for (g in 1:9) {
      C_pred[i,t,g] = fit_beta0[i,g] + beta[i,1,t]*X[g,1]+beta[i,2,t]*X[g,2]+beta[i,3,t]*X[g,3]+beta[i,4,t]*X[g,4];
    }
  }
}
time_pred <- NPI_data(country_2)[,1]
time_pred <- as.Date(time_pred, format = "%Y-%m-%d")
median_C_pred = apply(C_pred, c(2, 3), median)
low_C_pred = apply(C_pred, c(2, 3), quantile, probs = c(0.025))
high_C_pred = apply(C_pred, c(2, 3), quantile, probs = c(0.975))

create_plot <- function(j) {
  
  summary_C_pred = cbind(median_C_pred[,j], low_C_pred[,j], high_C_pred[,j])
  contacts <- cbind(as.Date(time_pred), as.data.frame(summary_C_pred))
  colnames(contacts) <- c("time","q50","q5","q95")
  
  ggplot(contacts, aes(x = as.Date(time))) +
    geom_ribbon(aes(ymin = q5, ymax = q95, fill = "95% CI"), alpha = 0.2) +
    geom_line(aes(x = time, y = q50, color = "Median"), size = 1) +
    scale_color_manual(name = '', values = c("Median" = "#009E73")) +
    scale_fill_manual(name = '', values = c("95% CI" = "#009E73")) +
    labs(x = "time", y = y_labels[j]) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    theme(
      text = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      title = element_text(size = 16),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
} 

y_labels <- c(
  "Average number of contacts [18,45) to [18,45)",
  "Average number of contacts [18,45) to [45,64)",
  "Average number of contacts [18,45) to 65+",
  "Average number of contacts [45,64) to [18,45)",
  "Average number of contacts [45,64) to [45,64)",
  "Average number of contacts [45,64) to 65+",
  "Average number of contacts 65+ to [18,45)",
  "Average number of contacts 65+ to [45,64)",
  "Average number of contacts 65+ to 65+")

figure <- ggarrange(plotlist = lapply(1:9, create_plot),
                    labels = "AUTO",
                    ncol = 3, nrow = 3,
                    common.legend = TRUE,
                    legend = "bottom")
return(figure)
}
