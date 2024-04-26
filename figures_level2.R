diagnostics <- function(country,percentile) {
  
  loaded_fit <- load_reg_fit(country,percentile)
  
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


generate_beta_plot <- function(fit_data, country, percentile, label) {
  tvem_beta <- fitted_beta_coefficients(country,percentile)[,-c(2,4,6,8)]
  tvem_beta_w <- fitted_beta_coefficients(country,percentile)[,-c(3,5,7,9)]
  
  fit_beta <- fit_data$beta_pred[, , label]
  median_fit_beta = apply(fit_beta, 2, median)
  low_fit_beta = apply(fit_beta, 2, quantile, probs = c(0.025))
  high_fit_beta = apply(fit_beta, 2, quantile, probs = c(0.975))
  
  time_ext <- tvem_beta[,1]
  time_pred <- NPI_data(country)[,1]
  
  beta_pred <- data.frame(cbind(time_pred, median_fit_beta, low_fit_beta, high_fit_beta))
  colnames(beta_pred) <- c("time", "q50", "q5", "q95")
  beta_tvem <- data.frame(cbind(time_ext, tvem_beta[,label + 1]))
  colnames(beta_tvem) <- c("time", "tvem_beta")
  beta_tvem_w <- data.frame(cbind(time_ext, tvem_beta_w[,label + 1]))
  colnames(beta_tvem_w) <- c("time", "tvem_beta_w")
  
  merged_df <- merge(beta_tvem, beta_tvem_w, by = "time", all.x = TRUE)
  merged_df <- merge(beta_pred, merged_df, by = "time", all.x = TRUE)
  
  names_beta_fig <- c("home", "work","school","other")
  
  g <- ggplot(merged_df, aes(x = as.Date(time))) +
    geom_ribbon(aes(ymin = as.numeric(q5), ymax = as.numeric(q95), fill = "95% CI", group = 1), alpha = 0.2) +
    geom_line(aes(y = as.numeric(q50), color = "Median", group = 1), size = 1) +
    geom_point(aes(y = tvem_beta, color = "TVEM beta(median)"), size = 2, shape = 16, stroke = 0) +
    geom_point(aes(y = tvem_beta_w, color = "TVEM beta wave(median)"), size = 4, shape = 16, stroke = 0) +
    scale_color_manual(name = '', values = c("Median" = "#CC79A7", "TVEM beta(median)" = 'black', "TVEM beta wave(median)" = "#999999")) +
    scale_fill_manual(name = '', values = c("95% CI" = "#CC79A7")) +
    labs(x = "time", y = paste("beta", names_beta_fig[label])) +
    #scale_y_continuous(breaks = seq(floor(min(as.numeric(merged_df$q5))), ceiling(max(as.numeric(merged_df$q95))), by = 0.1)) +
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
  
  return(g)
}

fitted_median_beta <- function(country,percentile) {
  loaded_fit <- load_reg_fit(country,percentile)
  
  posts_1 <- rstan::extract(loaded_fit)
  
  figure <- ggarrange(
    generate_beta_plot(posts_1, country, percentile, 1),
    generate_beta_plot(posts_1, country, percentile, 2),
    generate_beta_plot(posts_1, country, percentile, 3),
    generate_beta_plot(posts_1, country, percentile, 4),
    labels = "AUTO",
    ncol = 2, nrow = 2,
    common.legend = TRUE,
    legend = "bottom"
  )
  
  return(figure)
}

pred_CM <- function(country,percentile){
  
  loaded_fit <- load_reg_fit(country,percentile)
  
  time_ext <- fitted_beta_coefficients(country,percentile)[,1]
  time_pred <- NPI_data(country)[,1]
  
  time_comix <- as.Date(comix_dates(country)[,1])
  date_column <- as.Date(time_comix)
  comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
  
  merged_data <- cbind(Date = as.data.frame(date_column), comix_matrix)
  min_date <- min(merged_data$date_column, na.rm = TRUE)
  max_date <- max(merged_data$date_column, na.rm = TRUE)
  
  # Fill in missing dates, including the minimum date
  df_complete <- complete(merged_data, date_column = seq(min_date, max_date, by = "day"))
  df_complete <- df_complete[,-1]
  
  num_rows <- length(time_pred)
  num_cols <- ncol(comix_matrix)
  param_names <- matrix("", nrow = num_rows, ncol = num_cols)
  for (j in 1:num_cols) {
    for (i in 1:num_rows) {
      param_names[i, j] <- sprintf("C_pred[%d,%d]", i, j)
    }
  }
  
  create_plot <- function(j) {
    C_pred <- cbind(as.Date(time_pred),
      as.data.frame(summary(loaded_fit, pars = param_names[, j], 
                            probs = c(0.05, 0.5, 0.95))$summary))
    colnames(C_pred) <- c("time","mean","se_mean","sd","q5","q50","q95", "n_eff","Rhat")
    
    comix_data <- data.frame(as.Date(time_ext$Date),unlist(df_complete[, j]))
    colnames(comix_data) <- c("time", "data")
    
    merged_df <- merge(C_pred, comix_data, by = "time", all.x = TRUE)
    
    ggplot(merged_df, aes(x = as.Date(time))) +
      geom_ribbon(aes(ymin = q5, ymax = q95, fill = "95% CI"), alpha = 0.2) +
      geom_line(aes(x = time, y = q50, color = "Median"), size = 1) +
      geom_point(aes(y = data, color = "CoMix"), size = 4, shape = 16, stroke = 0) +
      scale_color_manual(name = '', values = c("Median" = "#009E73", "CoMix" = 'black')) +
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
  
  figure <- ggarrange(plotlist = lapply(1:num_cols, create_plot),
            labels = "AUTO",
            ncol = 3, nrow = 3,
            common.legend = TRUE,
            legend = "bottom")
  
  return(figure)
}

pred_CM_full <- function(country){
  
  loaded_fit_M <- load_reg_fit(country,"50")
  loaded_fit_L <- load_reg_fit(country,"2.5")
  loaded_fit_H <- load_reg_fit(country,"97.5")
  
  time_ext <- fitted_beta_coefficients(country,"50")[,1]
  time_pred <- NPI_data(country)[,1]
  
  time_comix <- as.Date(comix_dates(country)[,1])
  date_column <- as.Date(time_comix)
  comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
  
  merged_data <- cbind(Date = as.data.frame(date_column), comix_matrix)
  min_date <- min(merged_data$date_column, na.rm = TRUE)
  max_date <- max(merged_data$date_column, na.rm = TRUE)
  
  # Fill in missing dates, including the minimum date
  df_complete <- complete(merged_data, date_column = seq(min_date, max_date, by = "day"))
  df_complete <- df_complete[,-1]
  
  num_rows <- length(time_pred)
  num_cols <- ncol(comix_matrix)
  param_names <- matrix("", nrow = num_rows, ncol = num_cols)
  for (j in 1:num_cols) {
    for (i in 1:num_rows) {
      param_names[i, j] <- sprintf("C_pred[%d,%d]", i, j)
    }
  }
  
  create_plot <- function(j) {
    C_pred_M <- cbind(as.Date(time_pred),
                    as.data.frame(summary(loaded_fit_M, pars = param_names[, j], 
                                          probs = c(0.05, 0.5, 0.95))$summary))
    colnames(C_pred_M) <- c("time","mean_M","se_mean_M","sd_M","q5_M","q50_M","q95_M", "n_eff_M","Rhat_M")
    
    
    C_pred_L <- cbind(as.Date(time_pred),
                    as.data.frame(summary(loaded_fit_L, pars = param_names[, j], 
                                          probs = c(0.05, 0.5, 0.95))$summary))
    colnames(C_pred_L) <- c("time","mean_L","se_mean_L","sd_L","q5_L","q50_L","q95_L", "n_eff_L","Rhat_L")
    
    C_pred_H <- cbind(as.Date(time_pred),
                    as.data.frame(summary(loaded_fit_H, pars = param_names[, j], 
                                          probs = c(0.05, 0.5, 0.95))$summary))
    colnames(C_pred_H) <- c("time","mean_H","se_mean_H","sd_H","q5_H","q50_H","q95_H", "n_eff_H","Rhat_H")
    
    comix_data <- data.frame(as.Date(time_ext$Date),unlist(df_complete[, j]))
    colnames(comix_data) <- c("time", "data")
    
    merged_df <- merge(C_pred_M, C_pred_L, by = "time", all.x = TRUE)
    merged_df <- merge(merged_df, C_pred_H, by = "time", all.x = TRUE)
    merged_df <- merge(merged_df, comix_data, by = "time", all.x = TRUE)
    
    ggplot(merged_df, aes(x = as.Date(time))) +
      geom_ribbon(aes(ymin = q5_L, ymax = q95_H, fill = "quantile range"), alpha = 0.1) +
      geom_ribbon(aes(ymin = q5_M, ymax = q95_M, fill = "95% CI"), alpha = 0.2) +
      geom_line(aes(x = time, y = q50_M, color = "Median"), size = 1) +
      geom_point(aes(y = data, color = "CoMix"), size = 4, shape = 16, stroke = 0) +
      scale_color_manual(name = '', values = c("Median" = "#009E73", "CoMix" = 'black')) +
      scale_fill_manual(name = '', values = c("95% CI" = "#009E73", "quantile range"="#009E73")) +
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
  
  figure <- ggarrange(plotlist = lapply(1:num_cols, create_plot),
                      labels = "AUTO",
                      ncol = 3, nrow = 3,
                      common.legend = TRUE,
                      legend = "bottom")
  
  return(figure)
}







