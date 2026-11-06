#' Set up POLYMOD contact matrices for all countries
#' 
#' This function creates country-specific contact matrices by loading raw contact data,
#' aggregating it into specific age groups, and saving the results as CSV files.
#' Each country's data is processed and saved using its ISO 2-letter code.
#' 
#' @param age_group_labels Character vector. Labels for the target age groups.
#'                        Default is c("[0-10)", "[10,20)", "[20,25)", "[25,45)", "[45,65)", "65+").
#' @param age_group_mapping Character vector. Mapping from original 16 POLYMOD groups to target groups.
#'                         If NULL, will be auto-generated based on age_group_labels.
polymod_setup <- function(age_group_labels = c("[0-10)", "[10,20)", "[20,25)", "[25,45)", "[45,65)", "65+"),
                          age_group_mapping = NULL) {
  
  # Check if required data files exist
  required_files <- c(
    "data/contact_matrices_home.rda",
    "data/contact_matrices_work.rda",
    "data/contact_matrices_school.rda", 
    "data/contact_matrices_other_locations.rda"
  )
  
  for (file in required_files) {
    if (!file.exists(file)) {
      stop(paste("Required file not found:", file))
    }
  }
  
  # Load all necessary contact matrices from data files
  load("data/contact_matrices_home.rda")
  load("data/contact_matrices_work.rda")
  load("data/contact_matrices_school.rda")
  load("data/contact_matrices_other_locations.rda")
  
  # Define mapping of country names to their ISO 2-letter codes
  countries <- c(
    "Austria" = "AT",
    "Belgium" = "BE",
    "Croatia" = "HR",
    "Denmark" = "DK",
    "Estonia" = "EE",
    "Finland" = "FI",
    "France" = "FR",
    "Greece" = "GR",
    "Hungary" = "HU",
    "Italy" = "IT",
    "Lithuania" = "LT",
    "Netherlands" = "NL",
    "Poland" = "PL",
    "Portugal" = "PT",
    "Slovakia" = "SK",
    "Slovenia" = "SI",
    "Spain" = "ES",
    "United Kingdom" = "UK",
    "Bulgaria" = "BG",
    "Cyprus" = "CY",
    "Czechia" = "CZ",
    "Germany" = "DE",
    "Iceland" = "IS",
    "Ireland" = "IE",
    "Latvia" = "LV",
    "Luxembourg" = "LU",
    "Malta" = "MT",
    "Romania" = "RO",
    "Sweden" = "SE"
  )
  
  # Define the standard age groups used in the original POLYMOD data (16 groups)
  original_age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                           "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                           "60-64", "65-69", "70-74", "75+")
  
  # Create default age group mapping if not provided
  if (is.null(age_group_mapping)) {
    n_age_groups <- length(age_group_labels)
    
    if (n_age_groups == 6) {
      # Special case for 6 groups matching your specification
      age_group_mapping <- c(
        rep(age_group_labels[1], 2),    # 0-4, 5-9 -> [0-10)
        rep(age_group_labels[2], 2),    # 10-14, 15-19 -> [10,20)
        rep(age_group_labels[3], 1),    # 20-24  -> [20,25)
        rep(age_group_labels[4], 4),    # 25-29, 30-34, 35-39, 40-44  -> [25,45)
        rep(age_group_labels[5], 4),    # 45-49, 50-54, 55-59, 60-64  -> [45,65)
        rep(age_group_labels[6], 3)     # 65-69, 70-74, 75+ -> 65+
      )
    } else if (n_age_groups == 4) {
      # Legacy 4-group mapping
      age_group_mapping <- c(
        rep(age_group_labels[1], 4),    # 0-4 to 15-19
        rep(age_group_labels[2], 5),    # 20-24 to 40-44
        rep(age_group_labels[3], 4),    # 45-49 to 60-64
        rep(age_group_labels[4], 3)     # 65-69 to 75+
      )
    } else {
      # General case: distribute groups as evenly as possible
      age_group_mapping <- rep(age_group_labels, times = ceiling(16/n_age_groups))[1:16]
    }
  }
  
  # Validate mapping
  if (length(age_group_mapping) != 16) {
    stop("age_group_mapping must have exactly 16 elements (one for each original POLYMOD age group)")
  }
  
  # Directory to save the files
  folder_path <- "data/POLYMOD"
  
  # Create the directory if it doesn't exist
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  
  # Process each country's data
  for (country in names(countries)) {
    country_code <- countries[country]
    
    # Handle special cases with different naming conventions in datasets
    dataset_country <- if (country == "United Kingdom") "United Kingdom of Great Britain" 
    else if (country == "Czechia") "Czech Republic"
    else country
    
    # Extract contact matrices for each setting (home, work, school, other)
    conmat_home <- data.frame(contact_matrices_home[[dataset_country]])
    conmat_work <- data.frame(contact_matrices_work[[dataset_country]])
    conmat_school <- data.frame(contact_matrices_school[[dataset_country]])
    conmat_other_locations <- data.frame(contact_matrices_other_locations[[dataset_country]])
    
    # Set row and column names to original age groups
    rownames(conmat_home) <- colnames(conmat_home) <- original_age_groups
    rownames(conmat_work) <- colnames(conmat_work) <- original_age_groups
    rownames(conmat_school) <- colnames(conmat_school) <- original_age_groups
    rownames(conmat_other_locations) <- colnames(conmat_other_locations) <- original_age_groups
    
    # Get age distribution for the country using Bernadette package
    age_distr <- Bernadette::age_distribution(country = country, year = 2020)
    
    # Create a mapping table for aggregation
    lookup_table <- data.frame(Initial = age_distr$AgeGrp,
                               Mapping = age_group_mapping)
    
    # Aggregate age distribution and contact matrices
    aggr_age <- Bernadette::aggregate_age_distribution(age_distr, lookup_table)
    aggr_cm_home <- Bernadette::aggregate_contact_matrix(conmat_home, lookup_table, aggr_age)
    aggr_cm_work <- Bernadette::aggregate_contact_matrix(conmat_work, lookup_table, aggr_age)
    aggr_cm_school <- Bernadette::aggregate_contact_matrix(conmat_school, lookup_table, aggr_age)
    aggr_cm_other_locations <- Bernadette::aggregate_contact_matrix(conmat_other_locations, lookup_table, aggr_age)
    
    # Ensure consistent ordering of age groups in matrices
    aggr_cm_home <- aggr_cm_home[age_group_labels, age_group_labels]
    aggr_cm_work <- aggr_cm_work[age_group_labels, age_group_labels]
    aggr_cm_school <- aggr_cm_school[age_group_labels, age_group_labels]
    aggr_cm_other_locations <- aggr_cm_other_locations[age_group_labels, age_group_labels]
    
    # Combine the matrices by flattening each matrix and binding them row-wise
    polymod <- rbind(unlist(aggr_cm_home),
                     unlist(aggr_cm_work),
                     unlist(aggr_cm_school),
                     unlist(aggr_cm_other_locations))
    
    # Generate the file path
    file_path <- file.path(folder_path, paste0(country_code, ".csv"))
    
    # Write the data to a CSV file
    write.table(polymod, file = file_path, sep = ",", row.names = FALSE, col.names = TRUE)
    
    # Print a message
    message(paste("Saved", country, "data to", file_path, 
                  "with", length(age_group_labels), "age groups"))
  }
}

#' Load contact data for a specified country
#' 
#' This function loads POLYMOD and CoMix contact data for a specified country.
#' If the POLYMOD data doesn't exist, it runs the polymod_setup function to generate it.
#' If the CoMix data doesn't exist for the specified country, it uses Austria as a fallback.
#' 
#' @param country A character string representing the ISO 2-letter country code
#' @param age_group_labels Character vector. Labels for age groups. 
#'                        Only used if POLYMOD data needs to be generated.
#' @param age_group_mapping Character vector. Mapping for POLYMOD aggregation.
#'                         Only used if POLYMOD data needs to be generated.
#' @return A list containing two data frames: polymod and comix with contact matrices
contact_data <- function(country, 
                         age_group_labels = c("[0-12)", "[12,18)", "[18,25)", "[25,45)", "[45,65)", "65+"),
                         age_group_mapping = NULL) {
  
  polymod_path <- 'data/POLYMOD/'
  comix_path <- 'data/CoMix/'
  
  polymod_file <- file.path(polymod_path, paste0(country, '.csv'))
  comix_file <- file.path(comix_path, paste0(country, '.csv'))
  
  # Check if POLYMOD directory exists, if not create it
  if (!dir.exists(polymod_path)) {
    dir.create(polymod_path, recursive = TRUE)
  }
  
  # If POLYMOD file doesn't exist, generate it
  if (!file.exists(polymod_file)) {
    # Run the polymod_setup function to generate the needed files
    polymod_setup(age_group_labels = age_group_labels, 
                  age_group_mapping = age_group_mapping)
  }
  
  # If POLYMOD file still doesn't exist after setup, there's an issue
  if (!file.exists(polymod_file)) {
    warning(paste("Could not generate POLYMOD data for country code:", country))
    return(NULL)
  }
  
  # If CoMix file doesn't exist, use Austria as fallback
  if (!file.exists(comix_file)) {
    comix_file <- file.path(comix_path, "AT.csv")
    if (!file.exists(comix_file)) {
      warning("Neither requested country nor fallback Austria CoMix data exist.")
      return(NULL)
    }
  }
  
  polymod <- read.csv(polymod_file)
  
  # Handle special cases for Belgium and Netherlands 
  if (country=="BE"){
    comix <- read.csv(comix_file)
    comix <- comix[9:43,]
  }else if (country=="NL"){
    comix <- read.csv(comix_file)
    comix <- comix[9:28,]
  }else {
    comix <- read.csv(comix_file)
  }
  
  return(list(polymod = polymod, comix = comix))
  #return(list(polymod = polymod, comix = comix[1:30,])) #to test extrapolation
}

#' Convert contact data to a Stan model input list
#' 
#' This function takes a country code, loads the corresponding contact data,
#' and formats it into a list suitable for use with a Stan model.
#' 
#' @param country A character string representing the ISO 2-letter country code
#' @param age_group_labels Character vector. Labels for age groups.
#' @param age_group_mapping Character vector. Mapping for POLYMOD aggregation.
#' @return A list containing all necessary data for the Stan model
data_to_stan_list <- function(country, 
                              age_group_labels = c("[0-12)", "[12,18)", "[18,25)", "[25,45)", "[45,65)", "65+"),
                              age_group_mapping = NULL) {
  
  # Load contact data
  contact_data_result <- contact_data(country, age_group_labels, age_group_mapping)
  
  # Extract data dimensions
  comix_data <- contact_data_result[[2]][,-1]  # Remove first column (likely dates)
  polymod_data <- contact_data_result[[1]]
  
  # --- compute eta ---
  Y_mat <- as.matrix(comix_data)       # T × 36
  X_mat <- t(as.matrix(polymod_data))  # 36 × 4
  XtX_inv  <- solve(t(X_mat) %*% X_mat)
  c_matrix <- t(XtX_inv %*% t(X_mat) %*% t(Y_mat))     # T × 4
  Y_hat    <- t(X_mat %*% t(c_matrix))                  # T × 36
  eta_vec  <- colMeans(Y_mat - Y_hat)                   # 36-vector
  # -------
  
  # Get dimensions
  I <- ncol(comix_data)  # Number of age group pairs (n_age_groups^2)
  T <- nrow(comix_data)  # Number of time points
  K <- nrow(polymod_data)  # Number of settings (usually 4: home, work, school, other)
  
  # Create Stan data list
  stan_data_penalised <- list(
    I = I,
    T = T,
    K = K,
    num_knots = n_knots,
    spline_degree = s_degree,
    knots = unname(quantile(c(1:T), probs = seq(from = 0, to = 1, length.out = n_knots))),
    Y = as.matrix(comix_data),
    X = t(as.matrix(polymod_data)),
    eta  = eta_vec,     # NEW: 36-vector fixed intercept
    time = c(1:T),
    s_aa = n_aa,
    #s_tt0 = n_tt1, #for time-varying intercept
    s_tt1 = n_tt1,
    s_tt2 = n_tt2,
    s_tt3 = n_tt3,
    s_tt4 = n_tt4,
    s_ss = n_ss
  )
  
  return(stan_data_penalised)
}


#' Get the number of age groups from existing contact data
#' 
#' This helper function determines the number of age groups by examining existing data.
#' 
#' @param country A character string representing the ISO 2-letter country code
#' @return Integer representing the number of age groups, or NULL if data doesn't exist
get_n_age_groups <- function(country) {
  polymod_path <- 'data/POLYMOD/'
  polymod_file <- file.path(polymod_path, paste0(country, '.csv'))
  
  if (file.exists(polymod_file)) {
    polymod <- read.csv(polymod_file)
    # Calculate n_age_groups from the data dimensions
    # Each row represents one setting, columns represent age group pairs
    n_elements <- ncol(polymod)
    n_age_groups <- sqrt(n_elements)
    
    if (n_age_groups == round(n_age_groups)) {
      return(round(n_age_groups))
    }
  }
  
  return(NULL)
}

#' Get age group labels from the structure of contact data
#' 
#' This helper function generates appropriate age group labels based on the number of groups.
#' 
#' @param n_age_groups Integer. Number of age groups.
#' @return Character vector of age group labels
get_age_group_labels <- function(n_age_groups) {
  if (n_age_groups == 4) {
    return(c("[0-18)", "[18,45)", "[45,64)", "65+"))
  } else if (n_age_groups == 6) {
    return(c("[0-12)", "[12,18)", "[18,25)", "[25,45)", "[45,65)", "65+"))
  } else {
    # Generate generic labels
    return(paste0("Group_", 1:n_age_groups))
  }
}