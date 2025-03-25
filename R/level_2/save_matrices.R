#' Save Predicted Contact Matrices to CSV File
#'
#' This function extracts contact matrices from a fitted Stan model for a specific country
#' and percentile, and saves them to a CSV file for later use. The function computes summary
#' statistics from the posterior distribution of predicted contact rates.
#'
#' @param country Character string. The country code for which to extract contact matrices.
#' @param percentile Character string. The percentile level for prediction (e.g., "50", "2.5", "97.5").
#'
#' @return A data frame containing the extracted contact matrix data with the following columns:
#'         date, age_groups, mean_contacts, lower_95_ci, median_contacts, upper_95_ci.
save_contact_matrices <- function(country,percentile) {
  # Load required model and data
  loaded_fit <- load_reg_fit(country, percentile)
  time_ext <- fitted_beta_coefficients(country, percentile)
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
    "65+ to 65+")
  
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
  
  # Save to CSV
  output_file <- paste0('outputs_level2/CM/', country,'_', percentile, '_contact_matrices.csv')
  write.csv(contact_df, 
            file = output_file, 
            row.names = FALSE)
  
  return(contact_df)
}


#' Create Heatmap Visualizations of Predicted Contact Matrices
#'
#' This function creates heatmap visualizations of contact matrices for different
#' time points, allowing for visual inspection of how contact patterns change over time.
#'
#' @param country Character string. The country code for which to create heatmaps.
#' @param percentile Character string. The percentile level to use (e.g., "50", "2.5", "97.5").
#'
#' @return A list of ggplot objects, each representing a heatmap for a different date.
#'         The function also saves a combined plot of up to 9 heatmaps to a PNG file.
create_contact_matrix_heatmaps <- function(country, percentile) {
  # Get contact matrix data from CSV file
  contact_df <- read.csv(paste0('outputs_level2/CM/', country, '_', percentile, '_contact_matrices.csv'))
  
  age_groups <- c("0-18", "18-45", "45-64", "65+")
  # Reshape contact data into a 4x4 matrix for a specific date
  create_matrix <- function(date) {
    date_data <- contact_df[contact_df$date == date, ]
    matrix_data <- matrix(0, nrow = 4, ncol = 4)
    
    for (i in 1:nrow(date_data)) {
      from_to <- strsplit(as.character(date_data$age_groups[i]), " to ")[[1]]
      from_idx <- which(age_groups == from_to[1])
      to_idx <- which(age_groups == from_to[2])
      
      matrix_data[from_idx, to_idx] <- date_data$median_contacts[i]
    }
    
    return(matrix_data)
  }
  
  unique_dates <- unique(contact_df$date)
  
  # Create plots for each date
  plot_list <- lapply(unique_dates, function(current_date) {
    matrix_data <- create_matrix(current_date)
    melted_matrix <- melt(matrix_data)
    colnames(melted_matrix) <- c("From", "To", "Contacts")
    melted_matrix$FromGroup <- age_groups[melted_matrix$From]
    melted_matrix$ToGroup <- age_groups[melted_matrix$To]
    
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
  
  ggsave(filename = paste0('figures/heatmaps/', country, '_', percentile, '_contact_matrix_heatmaps.png'), 
         plot = combined_plot, 
         width = 20, 
         height = 20, 
         units = "cm")
  
  return(plot_list)
}