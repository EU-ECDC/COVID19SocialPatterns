diagnostics <- function(country) {
  
  loaded_fit <- load_reg_fit(country)
  
  hmc_diagnosis <- check_hmc_diagnostics(loaded_fit)
  
  traceplot_lp <- traceplot(loaded_fit, pars = c("lp__", "L_Omega", "L_sigma"), inc_warmup = TRUE, nrow = 2)
  traceplot_delta <- traceplot(loaded_fit, pars = c("delta0", "delta"), inc_warmup = TRUE, nrow = 4)
  
  stan_dens_lp <- stan_dens(loaded_fit, pars = c("lp__", "L_Omega", "L_sigma"), separate_chains = TRUE)
  stan_dens_delta <- stan_dens(loaded_fit, pars = c("delta0", "delta"), separate_chains = TRUE, nrow = 4)
  
  pairs_plot <- pairs(loaded_fit, pars = c("delta0", "delta"), labels = c("delta0", "delta"),
                      cex.labels = 1.5, font.labels = 9, condition = "accept_stat__")
  
  return(list(hmc_diagnosis = hmc_diagnosis,
              traceplot_lp = traceplot_lp,
              traceplot_beta = traceplot_beta,
              stan_dens_lp = stan_dens_lp,
              stan_dens_beta = stan_dens_beta,
              pairs_plot = pairs_plot))
}


generate_beta_plot <- function(fit_data, country, label) {
  fit_beta <- fit_data$y_pred[, , label]
  time_ext <- c(1:nrow(fitted_beta_coefficients(country)))
  median_fit_beta = apply(fit_beta, 2, median)
  low_fit_beta = apply(fit_beta, 2, quantile, probs = c(0.025))
  high_fit_beta = apply(fit_beta, 2, quantile, probs = c(0.975))
  
  beta_pred <- data.frame(cbind(time_ext, median_fit_beta, low_fit_beta, high_fit_beta, fitted_beta_coefficients(country)[, label + 1]))
  colnames(beta_pred) <- c("time", "q50", "q5", "q95", "data")
  
  g <- ggplot(beta_pred, aes(x = time)) +
    geom_ribbon(aes(ymin = q5, ymax = q95, fill = "95% CI"), alpha = 0.2) +
    geom_line(aes(y = q50, color = "Median"), size = 1) +
    geom_point(aes(y = data, color = "Median estimated beta"), size = 2, shape = 16, stroke = 0) +
    scale_color_manual(name = '', values = c("Median" = "#33a02c", "Median estimated beta" = 'black')) +
    scale_fill_manual(name = '', values = c("95% CI" = "#33a02c")) +
    labs(x = "time", y = paste("Median estimated beta", names(fitted_beta_coefficients(country)[, label + 1]))) +
    theme(
      text = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      title = element_text(size = 16),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    )
  
  return(g)
}

fitted_median_beta <- function(country) {
  loaded_fit <- load_reg_fit(country)
  
  posts_1 <- rstan::extract(loaded_fit)
  
  figure <- ggarrange(
    generate_beta_plot(posts_1, country, 1),
    generate_beta_plot(posts_1, country, 2),
    generate_beta_plot(posts_1, country, 3),
    generate_beta_plot(posts_1, country, 4),
    labels = "AUTO",
    ncol = 2, nrow = 2,
    common.legend = TRUE,
    legend = "bottom"
  )
  
  return(figure)
}