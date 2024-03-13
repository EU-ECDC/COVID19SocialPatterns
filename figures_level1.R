diagnostics <- function(country) {
  
  loaded_fit <- load_fit(country)
  
  hmc_diagnosis <- check_hmc_diagnostics(loaded_fit)
  
  traceplot_lp <- traceplot(loaded_fit, pars = c("lp__", "tau1", "tau2", "tau3", "tau4", "sigma"), inc_warmup = TRUE, nrow = 2)
  traceplot_beta <- traceplot(loaded_fit, pars = c("beta1", "beta2", "beta3", "beta4"), inc_warmup = TRUE, nrow = 4)
  
  stan_dens_lp <- stan_dens(loaded_fit, pars = c("lp__", "tau1", "tau2", "tau3", "tau4", "sigma"), separate_chains = TRUE)
  stan_dens_beta <- stan_dens(loaded_fit, pars = c("beta1", "beta2", "beta3", "beta4"), separate_chains = TRUE, nrow = 4)
  
  pairs_plot <- pairs(loaded_fit, pars = c("beta1", "beta2", "beta3", "beta4"), labels = c("beta1", "beta2", "beta3", "beta4"),
                      cex.labels = 1.5, font.labels = 9, condition = "accept_stat__")
  
  return(list(hmc_diagnosis = hmc_diagnosis,
              traceplot_lp = traceplot_lp,
              traceplot_beta = traceplot_beta,
              stan_dens_lp = stan_dens_lp,
              stan_dens_beta = stan_dens_beta,
              pairs_plot = pairs_plot))
}

fitted_beta <- function(country){
  
loaded_fit <- load_fit(country)  
comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
beta_labels <- c("Home", "Work", "School", "Other")

plot_list <- list()

for (i in seq_along(beta_labels)) {
  beta <- as.data.frame(summary(
    loaded_fit, pars = paste0("beta", i), probs = c(0.05, 0.5, 0.95))$summary)
  colnames(beta) <- make.names(colnames(beta))
  
  plot <- ggplot(beta, mapping = aes(x = c(1:nrow(comix_matrix)))) +
    geom_ribbon(aes(ymin = X5., ymax = X95.), alpha = 0.35) +
    geom_line(mapping = aes(x = c(1:nrow(comix_matrix)), y = X50.),) + 
    labs(x = "Wave", y = paste("beta", beta_labels[i]))
  
  plot_list[[i]] <- plot
}

figure1 <- ggarrange(plotlist = plot_list, 
          labels = "AUTO",
          ncol = 2, nrow = 2)

return(figure1)

}

fitted_contacts <- function(country){
  
  loaded_fit <- load_fit(country)  
  time <- c(1:nrow(contact_data(country)[[2]][,-1]))
  comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
  num_rows <- nrow(comix_matrix)
  num_cols <- ncol(comix_matrix)
  param_names <- matrix("", nrow = num_rows, ncol = num_cols)
  
  for (j in 1:num_cols) {
    for (i in 1:num_rows) {
      param_names[i, j] <- sprintf("y_pred[%d,%d]", i, j)
    }
  }
  
  create_plot <- function(j) {
    y_pred <- cbind(
      as.data.frame(summary(loaded_fit, pars = param_names[, j], 
                            probs = c(0.05, 0.5, 0.95))$summary),time, unlist(comix_matrix[, j]))
    colnames(y_pred) <- make.names(colnames(y_pred)) # Remove % in the column names
    
    ggplot(y_pred, aes(x = time)) +
      geom_ribbon(aes(ymin = X5., ymax = X95., fill = "95% CI"), alpha = 0.2) +
      geom_line(aes(x = time, y = X50., color = "Median"), size = 1) +
      geom_point(aes(y = unlist(comix_matrix[, j]), color = "CoMix"), size = 2, shape = 16, stroke = 0) +
      scale_color_manual(name = '', values = c("Median" = "#33a02c", "CoMix" = 'black')) +
      scale_fill_manual(name = '', values = c("95% CI" = "#33a02c")) +
      labs(x = "Wave", y = y_labels[j]) +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
      )
  }
 
  if (num_cols==9){
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
    
    figure2 <- ggarrange(plotlist = lapply(1:num_cols, create_plot),
                         labels = "AUTO",
                         ncol = 3, nrow = 3,
                         common.legend = TRUE,
                         legend = "bottom")
    
  } else if (num_cols==49){
    y_labels <- c(
      "Average number of contacts [0,12) to [0,12)",
      "Average number of contacts [0,12) to [12,18)",
      "Average number of contacts [0,12) to [18,25)",
      "Average number of contacts [0,12) to [25,45)",
      "Average number of contacts [0,12) to [45,65)",
      "Average number of contacts [0,12) to [65,85)",
      "Average number of contacts [0,12) to 85+",
      "Average number of contacts [12,18) to [0,12)",
      "Average number of contacts [12,18) to [12,18)",
      "Average number of contacts [12,18) to [18,25)",
      "Average number of contacts [12,18) to [25,45)",
      "Average number of contacts [12,18) to [45,65)",
      "Average number of contacts [12,18) to [65,85)",
      "Average number of contacts [12,18) to 85+",
      "Average number of contacts [18,25) to [0,12)",
      "Average number of contacts [18,25) to [12,18)",
      "Average number of contacts [18,25) to [18,25)",
      "Average number of contacts [18,25) to [25,45)",
      "Average number of contacts [18,25) to [45,65)",
      "Average number of contacts [18,25) to [65,85)",
      "Average number of contacts [18,25) to 85+",
      "Average number of contacts [25,45) to [0,12)",
      "Average number of contacts [25,45) to [12,18)",
      "Average number of contacts [25,45) to [18,25)",
      "Average number of contacts [25,45) to [25,45)",
      "Average number of contacts [25,45) to [45,65)",
      "Average number of contacts [25,45) to [65,85)",
      "Average number of contacts [25,45) to 85+",
      "Average number of contacts [45,65) to [0,12)",
      "Average number of contacts [45,65) to [12,18)",
      "Average number of contacts [45,65) to [18,25)",
      "Average number of contacts [45,65) to [25,45)",
      "Average number of contacts [45,65) to [45,65)",
      "Average number of contacts [45,65) to [65,85)",
      "Average number of contacts [45,65) to 85+",
      "Average number of contacts [65,85) to [0,12)",
      "Average number of contacts [65,85) to [12,18)",
      "Average number of contacts [65,85) to [18,25)",
      "Average number of contacts [65,85) to [25,45)",
      "Average number of contacts [65,85) to [45,65)",
      "Average number of contacts [65,85) to [65,85)",
      "Average number of contacts [65,85) to 85+",
      "Average number of contacts 85+ to [0,12)",
      "Average number of contacts 85+ to [12,18)",
      "Average number of contacts 85+ to [18,25)",
      "Average number of contacts 85+ to [25,45)",
      "Average number of contacts 85+ to [45,65)",
      "Average number of contacts 85+ to [65,85)",
      "Average number of contacts 85+ to 85+")
    
    figure2 <- ggarrange(plotlist = lapply(1:num_cols, create_plot),
                         labels = "AUTO",
                         ncol = 7, nrow = 7,
                         common.legend = TRUE,
                         legend = "bottom")}

return(figure2)
}
