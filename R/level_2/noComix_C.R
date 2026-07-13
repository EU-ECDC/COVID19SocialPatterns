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
generate_CM_predictions <- function(country_1, country_2, n_age_groups = NULL, save_matrices = FALSE, output_dir = 'outputs_level2/CM/') {
  # Load model fits from source country and extract posterior samples
  loaded_fit <- load_reg_fit(country_1, "50", n_age_groups)  # Add n_age_groups
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
  
  # # Load time-varying effective model (TVEM) fit to get baseline contact patterns
  # tvem_fit <- load_fit(country_1, n_age_groups)  # Add n_age_groups
  # posts_2 <- rstan::extract(tvem_fit)
  # fit_beta0 <- posts_2$beta0
  # X <- t(as.matrix(contact_data(country_2)[[1]][,]))
  
  # Compute eta for country_1 (fixed intercept from projection residuals)
  contact_data_1 <- contact_data(country_1)
  Y_mat   <- as.matrix(contact_data_1[[2]][,-1])         # T × I
  X_mat   <- t(as.matrix(contact_data_1[[1]][,]))        # I × K
  XtX_inv <- solve(t(X_mat) %*% X_mat)
  c_mat   <- t(XtX_inv %*% t(X_mat) %*% t(Y_mat))       # T × K
  eta_vec <- colMeans(Y_mat - t(X_mat %*% t(c_mat)))     # I-vector
  
  X <- t(as.matrix(contact_data(country_2)[[1]][,]))     # I × K
  
  # Infer age group structure if not provided
  if (is.null(n_age_groups)) {
    n_age_groups <- get_n_age_groups(country_1)
    if (is.null(n_age_groups)) {
      n_age_groups <- 4  # fallback
    }
  }
  
  n_contact_pairs <- n_age_groups * n_age_groups
  
  # Predict contact matrices - make this dynamic
  C_pred <- array(NA, dim = c(iterations, nrow(x_pred), n_contact_pairs))
  for (i in 1:iterations) {
    for (t in 1:nrow(x_pred)){
      for (g in 1:n_contact_pairs) {
        C_pred[i,t,g] = eta_vec[g] + beta[i,1,t]*X[g,1] + beta[i,2,t]*X[g,2] + 
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
    # Generate age group labels dynamically
    if (is.null(age_group_labels)) {
      age_group_labels <- get_age_group_labels(n_age_groups)
    }
    
    age_groups <- generate_age_pair_labels(age_group_labels)  # Use the function from figures_level1_new.R
    
    contact_df <- data.frame()
    # For each age group pair, extract and summarize posterior samples
    for (j in 1:n_contact_pairs) {
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
    
    # Save to CSV with age group suffix
    output_file <- paste0(output_dir, country_2, '_from_', country_1, '_', n_age_groups, 'groups_contact_matrices.csv')
    write.csv(contact_df, file = output_file, row.names = FALSE)
    
    message(paste("Contact matrices saved to:", output_file))
  }
  
  # Return results as a list
  return(list(
    time_pred = time_pred,
    median_C_pred = median_C_pred,
    low_C_pred = low_C_pred,
    high_C_pred = high_C_pred,
    C_pred = C_pred
  ))
}

#' Create Contact Matrix Plots from Saved CSV Files
#'
#' This function loads predicted contact matrices from a CSV file and creates
#' visualization plots showing the predicted contact patterns over time for
#' different age group pairs.
#'
#' @param csv_file_path Character string. Path to the CSV file containing predicted contact matrices.
#' @param n_age_groups Integer. Number of age groups. If NULL, will be inferred from data.
#' @param age_group_labels Character vector. Labels for age groups. If NULL, will be inferred.
#'
#' @return A ggplot2 arranged grid of plots, one for each age group pair, showing
#'         the predicted contact patterns over time with uncertainty intervals.
plot_CM_from_csv <- function(csv_file_path, n_age_groups = NULL, age_group_labels = NULL) {
  contact_df <- read.csv(csv_file_path)
  contact_df$date <- as.Date(contact_df$date)
  
  age_groups_in_data <- unique(contact_df$age_groups)
  
  # Infer age group structure if not provided
  if (is.null(n_age_groups)) {
    n_age_groups <- sqrt(length(age_groups_in_data))
    if (n_age_groups != round(n_age_groups)) {
      stop(paste("Cannot determine number of age groups from data. Found", 
                 length(age_groups_in_data), "age group pairs, which is not a perfect square."))
    }
    n_age_groups <- round(n_age_groups)
  }
  
  if (is.null(age_group_labels)) {
    age_group_labels <- get_age_group_labels(n_age_groups)
  }
  
  # Generate expected age group pairs dynamically
  groups_of_interest <- generate_age_pair_labels(age_group_labels)
  
  # Check if all expected groups exist in the data
  missing_groups <- groups_of_interest[!groups_of_interest %in% age_groups_in_data]
  if (length(missing_groups) > 0) {
    warning(paste("The following expected age group pairs are missing from the data:", 
                  paste(missing_groups, collapse = ", ")))
  }
  
  # Check if there are unexpected groups in the data
  unexpected_groups <- age_groups_in_data[!age_groups_in_data %in% groups_of_interest]
  if (length(unexpected_groups) > 0) {
    warning(paste("The following unexpected age group pairs found in data:", 
                  paste(unexpected_groups, collapse = ", ")))
  }
  
  # Get valid groups (those that exist in both expected and actual data)
  valid_groups <- groups_of_interest[groups_of_interest %in% age_groups_in_data]
  valid_indices <- match(valid_groups, groups_of_interest)
  
  grid_layout <- get_optimal_grid(length(valid_indices))
  n_groups    <- grid_layout$ncol  # square grid
  
  # Define function to create individual plots for each age group pair
  create_plot <- function(j) {
    row_i     <- ceiling(j / n_groups)
    col_i     <- ((j - 1) %% n_groups) + 1
    is_bottom <- row_i == n_groups
    is_right  <- col_i == n_groups
    
    # Inner "table" separators: drawn once per interior boundary, no outer frame.
    sep_col <- "grey40"
    sep_lw  <- 0.8
    
    current_group <- groups_of_interest[j]
    group_data <- contact_df[contact_df$age_groups == current_group, ]
    
    # Handle case where no data exists for this group
    if (nrow(group_data) == 0) {
      warning(paste("No data found for group:", current_group))
      return(NULL)
    }
    
    #y_label <- paste0(current_group)
    
    # Create plot
    ggplot(group_data, aes(x = date)) +
      geom_ribbon(aes(ymin = lower_95_ci, ymax = upper_95_ci, fill = "95% CI"), alpha = 0.2) +
      geom_line(aes(y = median_contacts, color = "Median"), size = 1) +
      scale_color_manual(name = '', values = c("Median" = "#009E73")) +
      scale_fill_manual(name  = '', values = c("95% CI" = "#009E73")) +
      labs(x = if (is_bottom) "time" else "", y = "") +
      scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
      # Inner "table" separators: right edge unless last column, bottom edge unless
      # last row. Drawn as panel annotations (not axis lines) so they are not removed
      # by patchwork's axis collection, and clipped off so edge lines render fully.
      {if (!is_right)  annotation_custom(grid::linesGrob(x = grid::unit(c(1, 1), "npc"),
                                                         y = grid::unit(c(0, 1), "npc"),
                                                         gp = grid::gpar(col = sep_col, lwd = sep_lw)))} +
      {if (!is_bottom) annotation_custom(grid::linesGrob(x = grid::unit(c(0, 1), "npc"),
                                                         y = grid::unit(c(0, 0), "npc"),
                                                         gp = grid::gpar(col = sep_col, lwd = sep_lw)))} +
      coord_cartesian(clip = "off") +
      theme(
        text         = element_text(size = 32),
        axis.text    = element_text(size = 30),
        axis.title   = element_text(size = 30),
        title        = element_text(size = 32),
        legend.text  = element_text(size = 32),
        legend.title = element_text(size = 32),
        axis.text.y  = element_text(size = 16, margin = margin(r = 2), hjust = 1),
        axis.title.x = if (is_bottom) element_text() else element_blank(),
        axis.text.x  = if (is_bottom) element_text(size = 12, angle = 45, hjust = 1)
        else element_blank(),
        axis.ticks.x = if (is_bottom) element_line() else element_blank()
      )
  }
  
  
  
  # Create plots only for valid groups
  plot_list <- lapply(valid_indices, create_plot)
  
  # Remove NULL plots (groups with no data)
  plot_list <- plot_list[!sapply(plot_list, is.null)]
  
  if (length(plot_list) == 0) {
    stop("No valid plots could be created. Check your data and age group configuration.")
  }
  
  figure_inner <- (wrap_plots(plot_list,
                              ncol = grid_layout$ncol,
                              nrow = grid_layout$nrow) +
                     plot_layout(guides = "collect", axes = "collect_x")) &
    theme(legend.position = "bottom")
  
  col_label_grobs <- lapply(age_group_labels, function(lbl) {
    wrap_elements(grid::textGrob(lbl, gp = grid::gpar(fontsize = 20, fontface = "bold"))) +
      theme(plot.margin = margin(0, 0, 0, 0))
  })
  col_strip <- Reduce(`+`, col_label_grobs) + plot_layout(ncol = n_groups)
  
  row_label_grobs <- lapply(age_group_labels, function(lbl) {
    wrap_elements(grid::textGrob(lbl, rot = 90, gp = grid::gpar(fontsize = 20, fontface = "bold"))) +
      theme(plot.margin = margin(0, 0, 0, 0))
  })
  row_strip <- Reduce(`/`, row_label_grobs) + plot_layout(nrow = n_groups)
  
  main_with_rows <- (row_strip | figure_inner) +
    plot_layout(widths = c(0.05, 1)) &
    theme(plot.margin = margin(1, 1, 1, 1))
  
  figure <- (col_strip / main_with_rows) +
    plot_layout(heights = c(0.04, 1)) &
    theme(plot.margin = margin(1, 1, 1, 1))
  
  return(figure)
}