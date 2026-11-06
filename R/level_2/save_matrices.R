#' Save Predicted Contact Matrices to CSV File
#'
#' This function extracts contact matrices from a fitted Stan model for a specific country
#' and percentile, and saves them to a CSV file for later use. The function computes summary
#' statistics from the posterior distribution of predicted contact rates.
#'
#' @param country Character string. The country code for which to extract contact matrices.
#' @param percentile Character string. The percentile level for prediction (e.g., "50", "2.5", "97.5").
#' @param n_age_groups Integer. Number of age groups. If NULL, will be inferred from data.
#' @param age_group_labels Character vector. Labels for age groups. If NULL, will be inferred.
#'
#' @return A data frame containing the extracted contact matrix data with the following columns:
#'         date, age_groups, mean_contacts, lower_95_ci, median_contacts, upper_95_ci.
save_contact_matrices <- function(country, percentile, n_age_groups = NULL, age_group_labels = NULL) {
  # Load required model and data
  loaded_fit <- load_reg_fit(country, percentile, n_age_groups)
  time_ext <- fitted_beta_coefficients(country, percentile, n_age_groups)
  time_pred <- NPI_data(country)[,1]
  
  # Extract posterior samples
  c_pred_samples <- rstan::extract(loaded_fit, pars = "C_pred")[[1]]
  
  num_rows <- dim(c_pred_samples)[2]
  num_cols <- dim(c_pred_samples)[3]
  
  # Infer age group structure if not provided
  if (is.null(n_age_groups)) {
    n_age_groups <- sqrt(num_cols)
    if (n_age_groups != round(n_age_groups)) {
      stop("Cannot determine number of age groups from data dimensions")
    }
    n_age_groups <- round(n_age_groups)
  }
  
  if (is.null(age_group_labels)) {
    age_group_labels <- get_age_group_labels(n_age_groups)
  }
  
  # Generate age group contact pairs dynamically
  age_groups <- character(num_cols)
  idx <- 1
  for (i in 1:n_age_groups) {
    for (j in 1:n_age_groups) {
      age_groups[idx] <- paste(age_group_labels[i], "to", age_group_labels[j])
      idx <- idx + 1
    }
  }
  
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
  
  # Save to CSV with age group suffix if specified
  if (!is.null(n_age_groups)) {
    output_file <- paste0('outputs_level2/CM/', country,'_', percentile, '_', n_age_groups, 'groups_contact_matrices.csv')
  } else {
    output_file <- paste0('outputs_level2/CM/', country,'_', percentile, '_contact_matrices.csv')
  }
  
  # Create directory if it doesn't exist
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  
  write.csv(contact_df, 
            file = output_file, 
            row.names = FALSE)
  
  message(paste("Contact matrices saved to:", output_file))
  
  return(contact_df)
}


#' Create Heatmap Visualizations of Predicted Contact Matrices
#'
#' This function creates heatmap visualizations of contact matrices for different
#' time points, allowing for visual inspection of how contact patterns change over time.
#'
#' @param country Character string. The country code for which to create heatmaps.
#' @param percentile Character string. The percentile level to use (e.g., "50", "2.5", "97.5").
#' @param n_age_groups Integer. Number of age groups. If NULL, will be inferred from data.
#' @param age_group_labels Character vector. Labels for age groups. If NULL, will be inferred.
#'
#' @return A list of ggplot objects, each representing a heatmap for a different date.
#'         The function also saves a combined plot of up to 9 heatmaps to a PNG file.
create_contact_matrix_heatmaps <- function(country, percentile, n_age_groups = NULL, age_group_labels = NULL) {
  
  # Determine filename based on age groups
  if (!is.null(n_age_groups)) {
    csv_file <- paste0('outputs_level2/CM/', country, '_', percentile, '_', n_age_groups, 'groups_contact_matrices.csv')
  } else {
    csv_file <- paste0('outputs_level2/CM/', country, '_', percentile, '_contact_matrices.csv')
  }
  
  # Try to read the CSV file
  if (!file.exists(csv_file)) {
    # Try the alternative naming convention
    csv_file <- paste0('outputs_level2/CM/', country, '_', percentile, '_contact_matrices.csv')
  }
  
  if (!file.exists(csv_file)) {
    warning(paste("Contact matrix CSV file not found for", country, percentile))
    return(NULL)
  }
  
  # Get contact matrix data from CSV file
  contact_df <- read.csv(csv_file)
  
  # Infer age group structure if not provided
  if (is.null(n_age_groups)) {
    unique_groups <- unique(contact_df$age_groups)
    n_age_groups <- sqrt(length(unique_groups))
    if (n_age_groups != round(n_age_groups)) {
      stop("Cannot determine number of age groups from contact matrix data")
    }
    n_age_groups <- round(n_age_groups)
  }
  
  if (is.null(age_group_labels)) {
    age_group_labels <- get_age_group_labels(n_age_groups)
  }
  
  # Reshape contact data into an n_age_groups x n_age_groups matrix for a specific date
  create_matrix <- function(date) {
    date_data <- contact_df[contact_df$date == date, ]
    matrix_data <- matrix(0, nrow = n_age_groups, ncol = n_age_groups)
    
    for (i in 1:nrow(date_data)) {
      from_to <- strsplit(as.character(date_data$age_groups[i]), " to ")[[1]]
      from_idx <- which(age_group_labels == from_to[1])
      to_idx <- which(age_group_labels == from_to[2])
      
      if (length(from_idx) > 0 && length(to_idx) > 0) {
        matrix_data[from_idx, to_idx] <- date_data$median_contacts[i]
      }
    }
    
    return(matrix_data)
  }
  
  unique_dates <- unique(contact_df$date)
  
  # Create plots for each date
  plot_list <- lapply(unique_dates, function(current_date) {
    matrix_data <- create_matrix(current_date)
    melted_matrix <- melt(matrix_data)
    colnames(melted_matrix) <- c("From", "To", "Contacts")
    melted_matrix$FromGroup <- age_group_labels[melted_matrix$From]
    melted_matrix$ToGroup <- age_group_labels[melted_matrix$To]
    
    # Create heatmap
    ggplot(melted_matrix, aes(x = ToGroup, y = FromGroup, fill = Contacts)) +
      geom_tile() +
      scale_fill_viridis(name = "Contacts") +
      labs(title = paste("Contact Matrix for", current_date),
           x = "Age Group", 
           y = "Age Group") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Combine plots, limiting to first 9 dates to prevent overwhelming output
  combined_plot <- wrap_plots(plot_list[1:min(9, length(plot_list))], ncol = 3)
  
  # Save with age group suffix if specified
  if (!is.null(n_age_groups)) {
    output_file <- paste0('figures/heatmaps/', country, '_', percentile, '_', n_age_groups, 'groups_contact_matrix_heatmaps.png')
  } else {
    output_file <- paste0('figures/heatmaps/', country, '_', percentile, '_contact_matrix_heatmaps.png')
  }
  
  # Create directory if it doesn't exist
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  
  ggsave(filename = output_file, 
         plot = combined_plot, 
         width = 20, 
         height = 20, 
         units = "cm")
  
  message(paste("Heatmaps saved to:", output_file))
  
  return(plot_list)
}