#' Create a Map of European Countries with Contact Data Coverage
#'
#' This function generates a map of EU/EEA countries colored by the availability
#' of contact pattern data (POLYMOD and CoMix) in each country. The map uses 
#' colorblind-friendly colors for different categories of data availability.
#'
#' @param save_plot Logical. Whether to save the map to a file. Default is FALSE.
#' @param output_path Character string. Path where the map should be saved if save_plot is TRUE.
#'                   Default is "figures/eu_eea_contact_data_map.png".
#' @param plot_width Numeric. Width of the saved plot in inches. Default is 10.
#' @param plot_height Numeric. Height of the saved plot in inches. Default is 8.
#' @param legend_position Numeric vector. Position of the legend as c(x, y) coordinates
#'                       where x and y are between 0 and 1. Default is c(0.7, 0.5).
#' @param title Character string. Title for the map. Default is "Map of EU/EEA Countries by Contact Data Coverage".
#'
#' @return A ggplot2 object containing the map visualization.
create_eu_eea_contact_map <- function(save_plot = FALSE, 
                                      output_path = "figures/eu_eea_contact_data_map.png",
                                      plot_width = 10, 
                                      plot_height = 8,
                                      legend_position = c(0.7, 0.5),
                                      title = "Map of EU/EEA Countries by Contact Data Coverage") {
  
  # Load required packages if not already loaded
  if (!requireNamespace("sf", quietly = TRUE)) library(sf)
  if (!requireNamespace("ggplot2", quietly = TRUE)) library(ggplot2)
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) library(rnaturalearth)
  if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) library(rnaturalearthdata)
  if (!requireNamespace("dplyr", quietly = TRUE)) library(dplyr)
  
  # Get the map data for Europe (and part of Asia to include all EU/EEA countries)
  europe <- ne_countries(continent = c('Europe','Asia'), scale = 'medium', returnclass = 'sf')
  
  # List of EU/EEA countries to include in the map
  eu_eea_countries <- c("Albania", "Andorra",
                        "Austria", 
                        "Belarus",
                        "Belgium", 
                        "Bosnia and Herzegovina",
                        "Bulgaria", "Croatia", "Cyprus", 
                        "Czech Republic", "Denmark", 
                        "Faeroe Islands",
                        "Estonia", "Finland", 
                        "Åland Islands",
                        "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
                        "Italy", 
                        "Kosovo",
                        "Latvia", "Lithuania", "Luxembourg", 
                        "North Macedonia",
                        "Malta", 
                        "Monaco", "Moldova", "Montenegro",
                        "Netherlands", "Norway", "Poland", "Portugal", "Romania",
                        #"Russian Federation", 
                        "San Marino", "Serbia",
                        "Slovakia", "Slovenia", "Spain", "Sweden", 
                        "Switzerland", "Ukraine", "United Kingdom", "Isle of Man", "Guernsey", "Jersey",
                        "Liechtenstein")
  
  # Filter the map data to include only EU/EEA countries
  europe <- europe %>% filter(name_long %in% eu_eea_countries)
  
  # Prepare the category data for each country
  # This defines what type of contact pattern data is available for each country
  category_data <- data.frame(
    name = c("Albania", "Andorra",
             "Austria", 
             "Belarus",
             "Belgium", 
             "Bosnia and Herzegovina",
             "Bulgaria", "Croatia", "Cyprus", 
             "Czech Republic", "Denmark", 
             "Faeroe Islands",
             "Estonia", "Finland", 
             "Åland Islands",
             "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
             "Italy", 
             "Kosovo",
             "Latvia", "Lithuania", "Luxembourg", 
             "North Macedonia",
             "Malta", 
             "Monaco", "Moldova", "Montenegro",
             "Netherlands", "Norway", "Poland", "Portugal", "Romania",
             #"Russian Federation", 
             "San Marino", "Serbia",
             "Slovakia", "Slovenia", "Spain", "Sweden", 
             "Switzerland", "Ukraine", "United Kingdom", "Isle of Man", "Guernsey", "Jersey",
             "Liechtenstein"),
    category = c("not included","not included",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021", 
                 "not included",
                 "Countries with POLYMOD and CoMix sampled < Oct 2020 to >Oct 2021",
                 "not included",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021", 
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "not included",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "not included",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with POLYMOD but without CoMix",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 "Countries with POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "not included",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 "not included",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 "not included","not included","not included",
                 "Countries with POLYMOD and CoMix sampled < Oct 2020 to >Oct 2021",
                 "Countries without extended POLYMOD or CoMix",
                 "Countries with POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 #"not included",
                 "not included","not included",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021",
                 "not included","not included",
                 "Countries with POLYMOD and CoMix sampled between Oct 2020 and Oct 2021",
                 "not included","not included","not included",
                 "Countries without extended POLYMOD or CoMix")
  )
  
  # Merge the map data with the category data
  europe <- europe %>%
    left_join(category_data, by = c("name_long" = "name"))
  
  # Define colorblind-friendly colors for the different categories
  colorblind_colors <- c(
    "Countries with POLYMOD and CoMix sampled < Oct 2020 to >Oct 2021" = "#E69F00",
    "Countries with POLYMOD and CoMix sampled between Oct 2020 and Oct 2021" = "#56B4E9",
    "Countries with extended POLYMOD and CoMix sampled between Oct 2020 and Oct 2021" = "#009E73",
    "Countries with extended POLYMOD but without CoMix between Oct 2020 and Oct 2021" = "#F0E442",
    "Countries with POLYMOD but without CoMix" = "#0072B2",
    "Countries without extended POLYMOD or CoMix" = "#D55E00"
    #"not included" ='grey'
  )
  
  # Plot the map with ggplot2
  map_plot <- ggplot(data = europe) +
    geom_sf(aes(fill = category)) +
    scale_fill_manual(values = colorblind_colors) +
    theme_minimal() +
    theme(legend.position = legend_position,  # Adjustable legend position
          legend.title = element_blank(),    # Remove legend title
          legend.text = element_text(size = 8), # Adjust text size
          legend.key.size = unit(0.5, "lines")) + # Adjust legend key size
    labs(title = title)
  
  # Save the plot if requested
  if (save_plot) {
    # Create directory if it doesn't exist
    dir_path <- dirname(output_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    # Save the plot
    ggsave(
      filename = output_path,
      plot = map_plot,
      width = plot_width,
      height = plot_height,
      units = "in"
    )
    
    message(paste("Map saved to:", output_path))
  }
  
  # Return the plot object
  return(map_plot)
}

#' Generate Heatmaps of POLYMOD Contact Matrices
#'
#' This function creates and saves heatmap visualizations of POLYMOD contact matrices
#' for a specified country, broken down by setting (Home, Work, School, Other).
#'
#' @param country Character string. The country code for which to generate heatmaps.
#'
#' @return A list containing matrices by setting, or NULL if no POLYMOD data is available.
#'         Heatmaps are saved to a "heatmaps_{country}/figures" directory.
POLYMOD_heatmaps <- function(country) {
  # Get POLYMOD data for the specified country
  polymod_data <- contact_data(country)$polymod
  
  # Check if POLYMOD data is available for this country
  if (is.null(polymod_data)) {
    warning(paste("No POLYMOD data available for country:", country))
    return(NULL)
  }
  
  # Create directory for saving heatmaps if it doesn't exist
  dir_path <- paste0("heatmaps_", country)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  
  # Create figures subdirectory if it doesn't exist
  figures_path <- file.path(dir_path, "figures")
  if (!dir.exists(figures_path)) {
    dir.create(figures_path)
  }
  
  # Define settings based on the row order in polymod_setup function
  settings <- c("Home", "Work", "School", "Other")
  
  # Initialize a list to store the matrices
  matrices <- list()
  
  # Check if we have the expected 4 rows (one per setting)
  if (nrow(polymod_data) == 4) {
    # Process each row as a separate setting
    for (i in 1:nrow(polymod_data)) {
      row_data <- as.numeric(polymod_data[i, ])
      if (length(row_data) == 16) {
        matrix_4x4 <- matrix(row_data, nrow = 4, byrow = TRUE)
        matrices[[i]] <- matrix_4x4
      } else {
        warning(paste("Row", i, "for country", country, 
                      "has unexpected length", length(row_data)))
      }
    }
  } else {
    warning(paste("Expected 4 rows for country", country, 
                  "(one per setting), but found", nrow(polymod_data)))
    
    for (i in 1:nrow(polymod_data)) {
      row_data <- as.numeric(polymod_data[i, ])
      if (length(row_data) == 16) {
        matrix_4x4 <- matrix(row_data, nrow = 4, byrow = TRUE)
        matrices[[i]] <- matrix_4x4
      }
    }
  }
  
  # Define age group names
  row_col_names <- c("[0-18)", "[18,45)", "[45,64)", "65+")
  
  for (i in seq_along(matrices)) {
    dimnames(matrices[[i]]) <- list(row_col_names, row_col_names)
  }
  
  # Function to create heatmap
  create_heatmap <- function(mat, title = NULL) {
    mat_melted <- melt(mat)
    colnames(mat_melted) <- c("Row", "Column", "value")
    
    # Create plot
    plot <- ggplot(data = mat_melted, aes(x = Column, y = Row, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "#009E73", name = "Contact Rate") +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(),
            axis.ticks = element_blank(),
            legend.position = "right")
    
    # Add title if provided
    if (!is.null(title)) {
      plot <- plot + ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5, size = 14))
    }
    
    return(plot)
  }
  
  # Create and save heatmaps for each setting
  for (i in seq_along(matrices)) {
    # Determine the setting name (use default if we have more or fewer than expected)
    setting_name <- if (i <= length(settings)) settings[i] else paste("Matrix", i)
    matrix_title <- paste(setting_name)
    heatmap_plot <- create_heatmap(matrices[[i]], matrix_title)
    
    # Save the heatmap with setting in the filename
    filename <- paste0(figures_path, "/POLYMOD_heatmap_", 
                       country, "_", tolower(setting_name), ".png")
    
    ggsave(
      filename = filename,
      plot = heatmap_plot,
      width = 8,
      height = 7
    )
    
    cat("Saved POLYMOD heatmap for country:", country, 
        "- Setting:", setting_name, "\n")
  }
  
  # Return the list of matrices with their settings
  result <- list(matrices = matrices)
  if (length(matrices) <= length(settings)) {
    names(result$matrices) <- settings[1:length(matrices)]
  }
  
  return(result)
}

#' Generate Heatmaps over time of CoMix Contact Matrices
#'
#' This function creates and saves heatmap visualizations of CoMix contact matrices
#' for a specified country across multiple time points during the COVID-19 pandemic.
#'
#' @param country Character string. The country code for which to generate CoMix heatmaps.
#'
#' @return A list containing:
#'   \item{dates}{Vector of dates for which matrices are available}
#'   \item{matrices}{List of contact matrices for each date}
#'         Heatmaps are saved to a "heatmaps_{country}/figures" directory.
CoMix_heatmaps_real_time <- function(country) {
  # Load CoMix data
  time_comix <- as.Date(comix_dates(country)[,1])
  date_column <- as.Date(time_comix)
  comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
  num_cols <- ncol(comix_matrix)
  age_groups <- 4
  expected_cols <- age_groups * age_groups
  
  if (num_cols != expected_cols) {
    warning(paste("Expected", expected_cols, "columns but found", num_cols, 
                  "columns in the CoMix data. Check if the age groups are defined correctly."))
  }
  
  # Convert merged_data to a proper data frame
  merged_data <- data.frame(date_column = date_column, comix_matrix)
  
  # Ensure dates are properly formatted
  min_date <- as.Date(min(date_column, na.rm = TRUE))
  max_date <- as.Date(max(date_column, na.rm = TRUE))
  date_seq <- seq.Date(from = min_date, to = max_date, by = 1)
  
  # Fill in missing dates to create a complete time series
  df_complete <- tidyr::complete(merged_data, date_column = date_seq)
  
  # Fill in missing dates to create a complete time series
  #df_complete <- complete(merged_data, date_column = seq(min_date, max_date, by = "day"))
  
  # Transform df_complete into a list of matrices with corresponding dates
  list_of_matrices <- lapply(seq_len(nrow(df_complete)), function(i) {
    # Reshape the data into a 4x4 matrix
    matrix(as.numeric(df_complete[i, 2:(num_cols+1)]), nrow = age_groups, byrow = TRUE)
  })
  
  # Define proper row and column names for 4 age groups
  row_col_names <- c("[0-18)", "[18,45)", "[45,64)", "65+")
  
  # Assign row and column names to each matrix
  for (i in seq_along(list_of_matrices)) {
    dimnames(list_of_matrices[[i]]) <- list(row_col_names, row_col_names)
  }
  
  # Create directory for saving heatmaps if it doesn't exist
  dir_path <- paste0("heatmaps_", country)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  
  # Create figures subdirectory if it doesn't exist
  figures_path <- file.path(dir_path, "figures")
  if (!dir.exists(figures_path)) {
    dir.create(figures_path)
  }
  
  # Function to create heatmap
  create_heatmap <- function(mat, date) {
    mat_melted <- melt(mat)
    colnames(mat_melted) <- c("Row", "Column", "value")
    ggplot(data = mat_melted, aes(x = Column, y = Row, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "#0072B2", name = "Contact Rate") +
      labs(x = "", y = "", title = paste("Contact Matrix:", date)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }
  
  # Create and save heatmaps for each date
  for (i in seq_along(list_of_matrices)) {
    current_date <- format(df_complete$date_column[i], "%Y-%m-%d")
    
    # Handle NA values in the matrix for visualization
    current_matrix <- list_of_matrices[[i]]
    if (all(is.na(current_matrix))) {
      cat("Skipping date", current_date, "- all values are NA\n")
      next
    }
    
    heatmap_plot <- create_heatmap(current_matrix, current_date)
    
    # Save with date in filename
    ggsave(
      filename = paste0(figures_path, "/CoMix_heatmap_", country, "_", current_date, ".png"),
      plot = heatmap_plot,
      width = 7,
      height = 6
    )
    
    cat("Saved heatmap for date:", current_date, "\n")
  }
  
  # Return the list of matrices and their dates
  return(list(
    dates = df_complete$date_column,
    matrices = list_of_matrices
  ))
}
