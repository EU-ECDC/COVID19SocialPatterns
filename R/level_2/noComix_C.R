#' Generate Contact Matrix Predictions for a Country Based on Another Country
#'
#' This function predicts contact matrices for a target country (country_2) using estimates
#' from a source country (country_1).
#'
#' @param country_1 Character string. The source country with a fitted model (CoMix country).
#' @param country_2 Character string. The target country for which to construct contact matrices(no-CoMix country).
#' @param save_matrices Logical. Whether to save the predicted contact matrices to CSV files. Default is FALSE.
#' @param output_dir Character string. Directory path for saving output CSV files. Default is 'outputs_level2/CM/'.
#'
#' @return A list containing:
#'   \item{time_pred}{Vector of dates for the predictions}
#'   \item{median_C_pred}{Matrix of median predicted contact values across all age group pairs and dates}
#'   \item{low_C_pred}{Matrix of lower 2.5% quantile values for prediction intervals}
#'   \item{high_C_pred}{Matrix of upper 97.5% quantile values for prediction intervals}
#'   \item{C_pred}{Full array of posterior samples of predicted contacts}
#'
generate_CM_predictions <- function(country_1, country_2, save_matrices = FALSE, output_dir = 'outputs_level2/CM/') {
  # Load model fits from source country and extract posterior samples
  loaded_fit <- load_reg_fit(country_1, "50")
  posts_1 <- rstan::extract(loaded_fit)
  iterations <- 12000
  fit_delta0 <- posts_1$delta0
  fit_delta <- posts_1$delta
  fit_L_Sigma <- posts_1$L_Sigma
  
  # Calculate covariance matrix
  cov_matrix <- array(NA, dim = c(iterations, 4, 4))
  for (i in 1:iterations) {
    cov_matrix[i,,] <- fit_L_Sigma[i,,] %*% t(fit_L_Sigma[i,,])
  }
  
  # Prepare MCA factors from target country as predictors
  # country_2 is the target country for which we want to predict contacts
  x_pred <- (mca_factors(country_2))[,-1]
  x_pred <- as.matrix(x_pred)
  mu <- array(NA, dim = c(iterations, 4, nrow(x_pred)))
  beta <- array(NA, dim = c(iterations, 4, nrow(x_pred)))
  
  # Generate beta parameters
  for (i in 1:iterations) {
    for (t in 1:nrow(x_pred)) {
      mu[i, , t] <- fit_delta0[i, ] + fit_delta[i, , ] %*% x_pred[t, ]
      beta[i, , t] <- MASS::mvrnorm(1, mu = mu[i, , t], Sigma = cov_matrix[i,,])
    }
  }
  
  # Load time-varying effective model (TVEM) fit to get baseline contact patterns
  tvem_fit <- load_fit(country_1) 
  posts_2 <- rstan::extract(tvem_fit)
  fit_beta0 <- posts_2$beta0
  X <- t(as.matrix(contact_data(country_2)[[1]][,]))
  
  # Predict contact matrices
  C_pred <- array(NA, dim = c(iterations, nrow(x_pred), 16))
  for (i in 1:iterations) {
    for (t in 1:nrow(x_pred)){
      for (g in 1:16) {
        C_pred[i,t,g] = fit_beta0[i,g] + beta[i,1,t]*X[g,1] + beta[i,2,t]*X[g,2] + 
          beta[i,3,t]*X[g,3] + beta[i,4,t]*X[g,4]
      }
    }
  }
  
  # Get time data and calculate summary statistics
  time_pred <- NPI_data(country_2)[,1]
  time_pred <- as.Date(time_pred, format = "%Y-%m-%d")
  
  # Calculate summary statistics
  median_C_pred = apply(C_pred, c(2, 3), median)
  low_C_pred = apply(C_pred, c(2, 3), quantile, probs = c(0.025))
  high_C_pred = apply(C_pred, c(2, 3), quantile, probs = c(0.975))
  
  # Save contacts to CSV if requested
  if (save_matrices) {
    # Define age group contact pairs
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
    # For each age group pair, extract and summarize posterior samples
    for (j in 1:16) {
      column_samples <- C_pred[,,j]
      
      temp_df <- data.frame(
        date = rep(as.Date(time_pred), each = 1),
        age_groups = age_groups[j],
        mean_contacts = apply(column_samples, 2, mean),
        lower_95_ci = apply(column_samples, 2, quantile, probs = 0.05),
        median_contacts = apply(column_samples, 2, median),
        upper_95_ci = apply(column_samples, 2, quantile, probs = 0.95)
      )
      
      contact_df <- rbind(contact_df, temp_df)
    }
    
    # Create directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Save to CSV
    output_file <- paste0(output_dir, country_2, '_from_', country_1, '_contact_matrices.csv')
    write.csv(contact_df, file = output_file, row.names = FALSE)
    
    message(paste("Contact matrices saved to:", output_file))
  }
  
  # Return results as a list
  return(list(
    time_pred = time_pred,
    median_C_pred = median_C_pred,
    low_C_pred = low_C_pred,
    high_C_pred = high_C_pred,
    C_pred = C_pred  # Return the full prediction array for potential additional analyses
  ))
}

#' Create Contact Matrix Plots from Saved CSV Files
#'
#' This function loads predicted contact matrices from a CSV file and creates
#' visualization plots showing the predicted contact patterns over time for
#' different age group pairs.
#'
#' @param csv_file_path Character string. Path to the CSV file containing predicted contact matrices.
#'
#' @return A ggplot2 arranged grid of plots, one for each age group pair, showing
#'         the predicted contact patterns over time with uncertainty intervals.
plot_CM_from_csv <- function(csv_file_path) {
  contact_df <- read.csv(csv_file_path)
  
  contact_df$date <- as.Date(contact_df$date)
  
  age_groups <- unique(contact_df$age_groups)
  
  groups_of_interest <- c(
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
    current_group <- groups_of_interest[j]
    group_data <- contact_df[contact_df$age_groups == current_group, ]
    
    y_label <- paste0("Average number of contacts\n", current_group)
    
    # Create plot
    ggplot(group_data, aes(x = date)) +
      geom_ribbon(aes(ymin = lower_95_ci, ymax = upper_95_ci, fill = "95% CI"), alpha = 0.2) +
      geom_line(aes(y = median_contacts, color = "Median"), size = 1) +
      scale_color_manual(name = '', values = c("Median" = "#009E73")) +
      scale_fill_manual(name = '', values = c("95% CI" = "#009E73")) +
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
  
  # Check if all groups of interest exist in the data
  missing_groups <- groups_of_interest[!groups_of_interest %in% age_groups]
  if (length(missing_groups) > 0) {
    warning(paste("The following age groups are missing from the data:", 
                  paste(missing_groups, collapse = ", ")))
  }
  
  # Get valid groups (those that exist in the data)
  valid_groups <- groups_of_interest[groups_of_interest %in% age_groups]
  valid_indices <- match(valid_groups, groups_of_interest)
  
  # Create plots only for valid groups
  plot_list <- lapply(seq_along(valid_groups), function(i) create_plot(valid_indices[i]))
  
  # Calculate grid dimensions based on number of valid groups
  n_plots <- length(valid_groups)
  if (n_plots <= 3) {
    ncol <- n_plots
    nrow <- 1
  } else if (n_plots <= 6) {
    ncol <- 3
    nrow <- 2
  } else {
    ncol <- 3
    nrow <- 3
  }
  
  # Arrange plots in a grid
  figure <- ggarrange(plotlist = plot_list,
                      labels = "AUTO",
                      ncol = ncol, nrow = nrow,
                      common.legend = TRUE,
                      legend = "bottom")
  
  return(figure)
}