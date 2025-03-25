#' Generate Diagnostic Plots for a Multivariate Regression Stan Model
#'
#' This function loads a previously fitted Stan model and generates various diagnostic
#' plots to assess MCMC convergence and model fit.
#'
#' @param country A character string specifying the country code.
#' @param percentile A numeric value indicating the percentile threshold used in the model.
#'
#' @return A list containing various diagnostic plots and information:
#'   \item{hmc_diagnosis}{Results from HMC diagnostics check}
#'   \item{traceplot_lp}{Trace plots for log posterior and covariance parameters}
#'   \item{traceplot_beta}{Trace plots for delta parameters}
#'   \item{stan_dens_lp}{Density plots for log posterior and covariance parameters}
#'   \item{stan_dens_beta}{Density plots for delta parameters}
#'   \item{pairs_plot}{Pairs plot showing parameter correlations}
diagnostics <- function(country,percentile) {
  # Load the fitted model for the specified country and percentile
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
              traceplot_delta = traceplot_delta,
              stan_dens_lp = stan_dens_lp,
              stan_dens_delta = stan_dens_delta,
              pairs_plot = pairs_plot))
}

#' Generate Beta Coefficient Plot for a Specific Location(home, work, school and other places)
#'
#' This function creates a plot comparing predicted beta coefficients from the fitted multivariate regression
#' model(level 2) with the estimated posterior median of the beta coefficients from the Time-Varying Effect Model 
#' (level 1) in CoMix survey waves time (grey dots) and in real-time (black dots) for the different locations 
#' (home, work, school and other places).
#'
#' @param fit_data A list containing the fitted model's posterior samples.
#' @param country A character string specifying the country code.
#' @param percentile A numeric value indicating the percentile threshold used in the model.
#' @param label An integer specifying which beta coefficient to plot (1=Home, 2=Work, 3=School, 4=Other).
#'
#' @return A ggplot object showing the predicted beta coefficients with uncertainty intervals
#'         and TVEM beta coefficients for comparison.
generate_beta_plot <- function(fit_data, country, percentile, label) {
  # Extract TVEM beta coefficients
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
  
  names_beta_fig <- c("Home", "Work","School","Other")
  
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
      text = element_text(size = 32),
      axis.text = element_text(size = 30),
      axis.title = element_text(size = 30),
      title = element_text(size = 32),
      legend.text = element_text(size = 32),
      legend.title = element_text(size = 32),
      axis.text.x = element_text(size = 16,angle = 45, hjust = 1)
    )
  
  return(g)
}

#' Generate Combined Plot of Beta Coefficients for All Contact Settings
#'
#' This function creates a 2x2 grid of plots showing the beta coefficients for all
#' four contact settings (Home, Work, School, Other) for a given country and percentile.
#'
#' @param country A character string specifying the country code.
#' @param percentile A numeric value indicating the percentile threshold used in the model.
#'
#' @return A ggplot object arranged in a 2x2 grid showing beta coefficients for all four settings.
fitted_median_beta <- function(country,percentile) {
  # Load the fitted model for the specified country and percentile
  loaded_fit <- load_reg_fit(country,percentile)
  # Extract posterior samples from the fitted model
  posts_1 <- rstan::extract(loaded_fit)
  # Create a 2x2 grid of plots for the four beta coefficients
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


#' Predict and Plot Contact Matrices for a Specific Percentile
#'
#' This function predicts contact matrices for a given country and percentile level. It either
#' loads existing data from a CSV file or generates predictions from a fitted model. The function
#' then visualizes the predicted contact matrices along with CoMix observed data in a grid of plots.
#'
#' @param country Character string. The country code for which to predict contact matrices.
#' @param percentile Character string. The percentile level for prediction (e.g., "50", "2.5", "97.5").
#'
#' @return A ggplot object containing a grid of 16 plots arranged in a 4x4 layout, showing
#'         predicted contact matrices and CoMix data for each age group pair.
pred_CM <- function(country, percentile) {
  # Define the expected CSV file path
  csv_file <- paste0('outputs_level2/CM/', country, '_', percentile, '_contact_matrices.csv')
  
  # Check if the CSV file exists
  if (file.exists(csv_file)) {
    contact_df <- read.csv(csv_file)
    contact_df$date <- as.Date(contact_df$date)
    message("Using existing CSV file: ", csv_file)
  } else {
    # CSV doesn't exist, generate the data from loaded_fit
    message("CSV file not found. Generating contact matrices from model fit...")
    
    loaded_fit <- load_reg_fit(country, percentile)
    time_pred <- NPI_data(country)[,1]
    # Extract posterior samples
    c_pred_samples <- rstan::extract(loaded_fit, pars = "C_pred")[[1]]
    
    num_rows <- dim(c_pred_samples)[2]
    num_cols <- dim(c_pred_samples)[3]
    
    # Age group contact pairs
    age_groups <- c(
      "0-18 to 0-18",
      "0-18 to 18-45",
      "0-18 to 45-64",
      "0-18 to 65+",
      "18-45 to 0-18",
      "18-45 to 18-45",
      "18-45 to 45-64",
      "18-45 to 65+",
      "45-64 to 0-18",
      "45-64 to 18-45",
      "45-64 to 45-64",
      "45-64 to 65+",
      "65+ to 0-18",
      "65+ to 18-45",
      "65+ to 45-64",
      "65+ to 65+"
    )
    
    contact_df <- data.frame()
    # For each age group pair, compute summary statistics from posterior samples
    for (j in 1:num_cols) {
      column_samples <- c_pred_samples[,, j]
      
      temp_df <- data.frame(
        date = as.Date(time_pred),
        age_groups = age_groups[j],
        mean_contacts = apply(column_samples, 2, mean),
        lower_95_ci = apply(column_samples, 2, quantile, probs = 0.05),
        median_contacts = apply(column_samples, 2, median),
        upper_95_ci = apply(column_samples, 2, quantile, probs = 0.95)
      )
      
      contact_df <- rbind(contact_df, temp_df)
    }
  }
  
  time_ext <- fitted_beta_coefficients(country, percentile)[,1]
  time_comix <- as.Date(comix_dates(country)[,1])
  date_column <- as.Date(time_comix)
  comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
  
  merged_data <- cbind(Date = as.data.frame(date_column), comix_matrix)
  min_date <- min(merged_data$date_column, na.rm = TRUE)
  max_date <- max(merged_data$date_column, na.rm = TRUE)
  
  # Fill in missing dates, including the minimum date
  df_complete <- complete(merged_data, date_column = seq(min_date, max_date, by = "day"))
  df_complete <- df_complete[,-1]
  
  target_groups <- unique(contact_df$age_groups)
  # Define function to create individual plots for each age group pair
  create_plot <- function(j) {
    current_group <- target_groups[j]
    group_data <- contact_df[contact_df$age_groups == current_group, ]
    y_label <- paste0(current_group)
    
    # Convert to proper format for plotting
    C_pred <- data.frame(
      time = as.Date(group_data$date),
      mean = group_data$mean_contacts,
      q5 = group_data$lower_95_ci,
      q50 = group_data$median_contacts,
      q95 = group_data$upper_95_ci)
    
    # Get the corresponding CoMix data
    comix_data <- data.frame(
      time = as.Date(time_ext$Date),
      data = unlist(df_complete[, j]))
    
    # Merge the prediction and CoMix data
    merged_df <- merge(C_pred, comix_data, by = "time", all.x = TRUE)
    
    # Create the plot
    ggplot(merged_df, aes(x = as.Date(time))) +
      geom_ribbon(aes(ymin = q5, ymax = q95, fill = "95% CI"), alpha = 0.2) +
      geom_line(aes(x = time, y = q50, color = "Median"), size = 1) +
      geom_point(aes(y = data, color = "CoMix"), size = 4, shape = 16, stroke = 0) +
      scale_color_manual(name = '', values = c("Median" = "#009E73", "CoMix" = 'black')) +
      scale_fill_manual(name = '', values = c("95% CI" = "#009E73")) +
      labs(x = "time", y = y_label) +  # Use the specific y_label for this plot
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme(
        text = element_text(size = 32),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        title = element_text(size = 32),
        legend.text = element_text(size = 32),
        legend.title = element_text(size = 32),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1)
      )
  }
  
  # Create all plots and arrange them
  figure <- ggarrange(
    plotlist = lapply(1:length(target_groups), create_plot),
    labels = "AUTO",
    ncol = 4, nrow = 4,
    common.legend = TRUE,
    legend = "bottom"
  )
  
  return(figure)
}

#' Predict and Plot Contact Matrices with Multiple Percentile Ranges
#'
#' This function creates composite contact matrix plots that incorporate prediction
#' uncertainty from multiple percentile levels (2.5%, 50%, and 97.5%). It visualizes
#' predicted contact matrices alongside CoMix observed data to show the full range
#' of prediction uncertainty.
#'
#' @param country Character string. The country code for which to predict contact matrices.
#'
#' @return A ggplot object containing a grid of 16 plots arranged in a 4x4 layout, each showing
#'         predicted contact matrices with uncertainty ranges and CoMix data for each age group pair.
pred_CM_full <- function(country) {
  # Try to load CSV files first
  csv_file_50 <- paste0('outputs_level2/CM/', country, '_50_contact_matrices.csv')
  csv_file_2_5 <- paste0('outputs_level2/CM/', country, '_2.5_contact_matrices.csv')
  csv_file_97_5 <- paste0('outputs_level2/CM/', country, '_97.5_contact_matrices.csv')
  
  # Check if all files exist
  files_exist <- file.exists(csv_file_50) && file.exists(csv_file_2_5) && file.exists(csv_file_97_5)
  
  if (files_exist) {
    message("Using existing CSV files for ", country)
    contact_df_50 <- read.csv(csv_file_50)
    contact_df_2_5 <- read.csv(csv_file_2_5)
    contact_df_97_5 <- read.csv(csv_file_97_5)
    
    contact_df_50$date <- as.Date(contact_df_50$date)
    contact_df_2_5$date <- as.Date(contact_df_2_5$date)
    contact_df_97_5$date <- as.Date(contact_df_97_5$date)
    
    using_csv <- TRUE
  } else {
    message("CSV files not found for ", country, ". Loading model fits instead...")
    using_csv <- FALSE
    
    # Load Stan model fits
    loaded_fit_M <- load_reg_fit(country, "50")
    loaded_fit_L <- load_reg_fit(country, "2.5")
    loaded_fit_H <- load_reg_fit(country, "97.5")
  }
  
  time_ext <- fitted_beta_coefficients(country, "50")[,1]
  time_pred <- NPI_data(country)[,1]
  
  time_comix <- as.Date(comix_dates(country)[,1])
  date_column <- as.Date(time_comix)
  comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
  
  merged_data <- cbind(Date = as.data.frame(date_column), comix_matrix)
  min_date <- min(merged_data$date_column, na.rm = TRUE)
  max_date <- max(merged_data$date_column, na.rm = TRUE)
  
  # Create a complete time series with all days, filling in missing dates
  df_complete <- complete(merged_data, date_column = seq(min_date, max_date, by = "day"))
  df_complete <- df_complete[,-1]
  
  num_rows <- length(time_pred)
  num_cols <- ncol(comix_matrix)
  
  # Only needed for load_reg_fit method
  if (!using_csv) {
    param_names <- matrix("", nrow = num_rows, ncol = num_cols)
    for (j in 1:num_cols) {
      for (i in 1:num_rows) {
        param_names[i, j] <- sprintf("C_pred[%d,%d]", i, j)
      }
    }
  }
  
  # Define age groups matching the column order
  age_groups <- c(
    "0-18 to 0-18",
    "0-18 to 18-45",
    "0-18 to 45-64",
    "0-18 to 65+",
    "18-45 to 0-18",
    "18-45 to 18-45",
    "18-45 to 45-64",
    "18-45 to 65+",
    "45-64 to 0-18",
    "45-64 to 18-45",
    "45-64 to 45-64",
    "45-64 to 65+",
    "65+ to 0-18",
    "65+ to 18-45",
    "65+ to 45-64",
    "65+ to 65+"
  )
  
  # Define function to create individual plots for each age group pair
  create_plot <- function(j) {
    if (using_csv) {
      current_group <- age_groups[j]
      group_data_50 <- contact_df_50[contact_df_50$age_groups == current_group, ]
      group_data_2_5 <- contact_df_2_5[contact_df_2_5$age_groups == current_group, ]
      group_data_97_5 <- contact_df_97_5[contact_df_97_5$age_groups == current_group, ]
      
      # Create data frames for plotting
      C_pred_M <- data.frame(
        time = as.Date(group_data_50$date),
        mean_M = group_data_50$mean_contacts,
        se_mean_M = NA,
        sd_M = NA,
        q5_M = group_data_50$lower_95_ci,
        q50_M = group_data_50$median_contacts,
        q95_M = group_data_50$upper_95_ci
      )
      
      C_pred_L <- data.frame(
        time = as.Date(group_data_2_5$date),
        mean_L = group_data_2_5$mean_contacts,
        se_mean_L = NA,
        sd_L = NA,
        q5_L = group_data_2_5$lower_95_ci,
        q50_L = group_data_2_5$median_contacts,
        q95_L = group_data_2_5$upper_95_ci
      )
      
      C_pred_H <- data.frame(
        time = as.Date(group_data_97_5$date),
        mean_H = group_data_97_5$mean_contacts,
        se_mean_H = NA,
        sd_H = NA,
        q5_H = group_data_97_5$lower_95_ci,
        q50_H = group_data_97_5$median_contacts,
        q95_H = group_data_97_5$upper_95_ci
      )
      
    } else {
      # Use direct model fit approach - extract summary statistics from Stan fits
      C_pred_M <- cbind(as.Date(time_pred),
                        as.data.frame(summary(loaded_fit_M, pars = param_names[, j], 
                                              probs = c(0.05, 0.5, 0.95))$summary))
      colnames(C_pred_M) <- c("time", "mean_M", "se_mean_M", "sd_M", "q5_M", "q50_M", "q95_M", "n_eff_M", "Rhat_M")
      
      C_pred_L <- cbind(as.Date(time_pred),
                        as.data.frame(summary(loaded_fit_L, pars = param_names[, j], 
                                              probs = c(0.05, 0.5, 0.95))$summary))
      colnames(C_pred_L) <- c("time", "mean_L", "se_mean_L", "sd_L", "q5_L", "q50_L", "q95_L", "n_eff_L", "Rhat_L")
      
      C_pred_H <- cbind(as.Date(time_pred),
                        as.data.frame(summary(loaded_fit_H, pars = param_names[, j], 
                                              probs = c(0.05, 0.5, 0.95))$summary))
      colnames(C_pred_H) <- c("time", "mean_H", "se_mean_H", "sd_H", "q5_H", "q50_H", "q95_H", "n_eff_H", "Rhat_H")
    }
    
    # Get CoMix data
    comix_data <- data.frame(as.Date(time_ext$Date), unlist(df_complete[, j]))
    colnames(comix_data) <- c("time", "data")
  
    y_label <- paste0(age_groups[j])
    
    # Merge data
    merged_df <- merge(C_pred_M, C_pred_L, by = "time", all.x = TRUE)
    merged_df <- merge(merged_df, C_pred_H, by = "time", all.x = TRUE)
    merged_df <- merge(merged_df, comix_data, by = "time", all.x = TRUE)
    
    # Create plot
    ggplot(merged_df, aes(x = as.Date(time))) +
      geom_ribbon(aes(ymin = q5_L, ymax = q95_H, fill = "quantile range"), alpha = 0.1) +
      geom_ribbon(aes(ymin = q5_M, ymax = q95_M, fill = "95% CI"), alpha = 0.2) +
      geom_line(aes(x = time, y = q50_M, color = "Median"), size = 1) +
      geom_point(aes(y = data, color = "CoMix"), size = 4, shape = 16, stroke = 0) +
      scale_color_manual(name = '', values = c("Median" = "#009E73", "CoMix" = 'black')) +
      scale_fill_manual(name = '', values = c("95% CI" = "#009E73", "quantile range" = "#009E73")) +
      labs(x = "time", y = y_label) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme(
        text = element_text(size = 32),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 26),
        title = element_text(size = 32),
        legend.text = element_text(size = 32),
        legend.title = element_text(size = 32),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
      )
  }
  
  # Create all 16 plots and arrange them in a 4x4 grid
  figure <- ggarrange(
    plotlist = lapply(1:num_cols, create_plot),
    labels = "AUTO",
    ncol = 4, nrow = 4,
    common.legend = TRUE,
    legend = "bottom"
  )
  
  return(figure)
}