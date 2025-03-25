#==============================================================================
# DEPENDENT VARIABLE PROCESSING
#==============================================================================

#' Extract and Interpolate Beta Coefficients from Stan Fit
#'
#' This function extracts the posterior distributions of beta coefficients from a Stan model,
#' calculates summary statistics based on the requested percentile, and performs linear
#' interpolation to create a continuous trajectory.
#'
#' @param country Character string indicating the country to analyze
#' @param percentile Character string indicating which percentile to extract ("50", "2.5", or "97.5")
#' @return A tibble containing dates and interpolated beta coefficients
fitted_beta_coefficients <- function(country,percentile){
  # Load the Stan fit for the specified country
  loaded_fit <- load_fit(country)
  posts <-  rstan::extract(loaded_fit)
  
  # Extract median, lower and upper confidence interval values for each beta coefficient
  median_beta1 = c(apply(posts$beta1, 2, median),NA)
  median_beta2 = c(apply(posts$beta2, 2, median),NA)
  median_beta3 = c(apply(posts$beta3, 2, median),NA)
  median_beta4 = c(apply(posts$beta4, 2, median),NA)

  low_beta1 = c(apply(posts$beta1, 2, quantile, probs = c(0.025)),NA)
  low_beta2 = c(apply(posts$beta2, 2, quantile, probs = c(0.025)),NA)
  low_beta3 = c(apply(posts$beta3, 2, quantile, probs = c(0.025)),NA)
  low_beta4 = c(apply(posts$beta4, 2, quantile, probs = c(0.025)),NA)
  
  high_beta1 = c(apply(posts$beta1, 2, quantile, probs = c(0.975)),NA)
  high_beta2 = c(apply(posts$beta2, 2, quantile, probs = c(0.975)),NA)
  high_beta3 = c(apply(posts$beta3, 2, quantile, probs = c(0.975)),NA)
  high_beta4 = c(apply(posts$beta4, 2, quantile, probs = c(0.975)),NA)
  
  if (percentile=="50"){
    dependent <- cbind(median_beta1,median_beta2,median_beta3,median_beta4)
  }else if (percentile=="2.5"){
    dependent <- cbind(low_beta1,low_beta2,low_beta3,low_beta4)
  }else if (percentile=="97.5"){
  dependent <- cbind(high_beta1,high_beta2,high_beta3,high_beta4)
  }
  # Create time variables for interpolation
  times <- c(comix_dates(country)[,1],max(comix_dates(country)[,2]))
  ext_dates <- seq(min(comix_dates(country)[,1]), max(comix_dates(country)[,2]), by = "days")
  numeric_dates <- as.numeric(times - min(times)) + 1
  wave <- c(1:length(times))

  df1 <- tibble(x=numeric_dates,y=dependent[,1],wave)
  df2 <- tibble(x=numeric_dates,y=dependent[,2],wave)
  df3 <- tibble(x=numeric_dates,y=dependent[,3],wave)
  df4 <- tibble(x=numeric_dates,y=dependent[,4],wave)
  
  # Linear interpolation for betas
  df1 %>% 
    mutate(gradient = (lead(y)-y)/(lead(x)-x)) %>% 
    complete(x = x:max(x)) %>% 
    fill(wave) %>% 
    group_by(wave) %>% 
    fill(gradient)%>% 
    mutate(y2=first(y) + (x-first(x)) *gradient)%>%
    mutate(y2=ifelse(!is.na(y2),y2,y))->df1_ext
  
  df2 %>% 
    mutate(gradient = (lead(y)-y)/(lead(x)-x)) %>% 
    complete(x = x:max(x)) %>% 
    fill(wave) %>% 
    group_by(wave) %>% 
    fill(gradient)%>% 
    mutate(y2=first(y) + (x-first(x)) *gradient)%>%
    mutate(y2=ifelse(!is.na(y2),y2,y))->df2_ext
  
  df3 %>% 
    mutate(gradient = (lead(y)-y)/(lead(x)-x)) %>% 
    complete(x = x:max(x)) %>% 
    fill(wave) %>% 
    group_by(wave) %>% 
    fill(gradient)%>% 
    mutate(y2=first(y) + (x-first(x)) *gradient)%>%
    mutate(y2=ifelse(!is.na(y2),y2,y))->df3_ext
  
  df4 %>% 
    mutate(gradient = (lead(y)-y)/(lead(x)-x)) %>% 
    complete(x = x:max(x)) %>% 
    fill(wave) %>% 
    group_by(wave) %>% 
    fill(gradient)%>% 
    mutate(y2=first(y) + (x-first(x)) *gradient)%>%
    mutate(y2=ifelse(!is.na(y2),y2,y))->df4_ext
 
  # Combine all interpolated data
  dependent_ext <- tibble(ext_dates,
                        df1_ext$y, df1_ext$y2,
                        df2_ext$y,df2_ext$y2,
                        df3_ext$y,df3_ext$y2,
                        df4_ext$y,df4_ext$y2)%>% 
  filter(!is.na(df1_ext$y2))%>%
  rename(Date = ext_dates,
         beta1_w = `df1_ext$y`,
         beta1 = `df1_ext$y2`,
         beta2_w = `df2_ext$y`,
         beta2 = `df2_ext$y2`,
         beta3_w = `df3_ext$y`,
         beta3 = `df3_ext$y2`,
         beta4_w = `df4_ext$y`,
         beta4 = `df4_ext$y2`,)

return(dependent_ext)
}

#' Extract Intercept Values from Stan Fit
#'
#' This function extracts posterior distributions of intercept (beta0) values from
#' a Stan model for a given country and summarizes them at the specified percentile.
#'
#' @param country Character string indicating the country to analyze
#' @param percentile Character string indicating which percentile to extract ("50", "2.5", or "97.5")
#' @return A vector of intercept values
fitted_interc <- function(country,percentile){
  
  loaded_fit <- load_fit(country)
  posts <-  rstan::extract(loaded_fit)
  
  if (percentile=="50"){
    fit_interc <- apply(posts$beta0, 2, median)
  }else if (percentile=="2.5"){
    fit_interc <- apply(posts$beta0, 2, quantile, probs = c(0.025))
  }else if (percentile=="97.5"){
    fit_interc <- apply(posts$beta0, 2, quantile, probs = c(0.975))
  }
  return(fit_interc)
}


#==============================================================================
# INDEPENDENT VARIABLE PROCESSING - MCA ANALYSIS
#==============================================================================

#' Perform Multiple Correspondence Analysis on NPI Data
#'
#' This function extracts Non-Pharmaceutical Intervention (NPI) data for a specified
#' country, processes it, and performs Multiple Correspondence Analysis (MCA).
#'
#' @param country Character string indicating the country to analyze
#' @return A list containing the MCA object, dates, and count of variables used
mca_analysis <- function(country){
  # Get NPI data for the country
  NPI <- NPI_data(country)
  NPI$date <- as.Date(NPI$date)
  
  # Filter for Physical distancing measures
  pattern <- "Physical.distancing"
  matching_indices <- grepl(pattern, names(NPI))
  date_var_indices <- sapply(NPI, function(x) class(x) == "Date")
  NPI_PD <- NPI[, c(which(date_var_indices & !matching_indices)[1], which(matching_indices))]
  
  # Clean variable names by removing common prefix
  pattern_to_remove <- "Physical.distancing."
  NPI_PD <- NPI_PD %>%
    rename_at(vars(-matches("date")), ~gsub(pattern_to_remove, "", .))
  
  # Abbreviate long category names
  NPI_PD <- NPI_PD %>%
    rename_with(~ gsub("Closure.of.public.spaces", "CPS", .), 
                starts_with("Closure.of.public.spaces"))%>%
    rename_with(~ gsub("Private.gathering.restriction", "PrG", .), 
                starts_with("Private.gathering.restriction"))%>%
    rename_with(~ gsub("Public.gathering.restriction", "PbG", .), 
                starts_with("Public.gathering.restriction"))%>%
    rename_with(~ gsub("Closure.of.educational.institutions", "CEI", .), 
                starts_with("Closure.of.educational.institutions"))%>%
    rename_with(~ gsub("Stay.at.home", "SH", .), 
                starts_with("Stay.at.home"))%>%
    rename_with(~ gsub("Measures.for.special.populations", "SpP", .), 
                starts_with("Measures.for.special.populations"))%>%
    rename_with(~ gsub("Workplace.measures", "W", .), 
                starts_with("Workplace.measures"))
  
  # Recode values (0->1, 1->2, 2->3) and convert to character for MCA analysis
  NPI_PD <- NPI_PD %>%
    mutate(across(-date, 
                  ~ case_when(. == 0 ~ 1, 
                              . == 1 ~ 2, 
                              . == 2 ~ 3, 
                              TRUE ~ .))) %>%
    mutate(across(where(is.numeric), as.character))
  
  # Store dates
  dates <- NPI_PD$date
  
  # Remove date column for MCA
  NPI_PD <- NPI_PD[, -which(names(NPI_PD) %in% c("date"))]
  
  # Select variables with more than one unique value
  NPI_PD_var <- NPI_PD %>%
    dplyr::select(where(~ length(unique(.)) > 1))
  
  # Apply MCA
  mca1 = MCA(NPI_PD_var, graph = FALSE)
  
  # Return necessary elements
  return(list(mca1 = mca1, date = dates, var_count = ncol(NPI_PD_var)))
}

#' Extract MCA Object for a Country
#'
#' Helper function that runs the MCA analysis and returns just the MCA object.
#'
#' @param country Character string indicating the country to analyze
#' @return MCA object containing the analysis results
mca <- function(country){
  # Run the analysis and return the MCA object
  return(mca_analysis(country)$mca1)
}

#' Extract MCA Factors to be used as Independent variables in Regression Analysis
#'
#' This function extracts the factor coordinates from the MCA analysis
#' for a given country, handling cases with different numbers of dimensions.
#'
#' @param country Character string indicating the country to analyze
#' @return A data frame containing dates and MCA factor coordinates
mca_factors <- function(country){
  # Get results from the analysis
  results <- mca_analysis(country)
  mca1 <- results$mca1
  
  # Extract coordinates
  if (results$var_count >= 5){
    independent <- as.data.frame(mca1$ind$coord[, 1:5])
  } else {
    independent <- as.data.frame(mca1$ind$coord[, 1:min(4, ncol(mca1$ind$coord))])
  }
  
  # Add date column
  independent <- cbind(results$date, independent)
  colnames(independent)[1] <- "date"
  
  return(independent)
}


#' Calculate Percentage of Variance Explained
#'
#' This function calculates the percentage of variance explained by the first 5 dimensions
#' (or fewer if not available) of the MCA analysis for a given country.
#'
#' @param country Character string indicating the country to analyze
#' @return Numeric value representing the percentage of variance explained
percentage_var <- function(country){
  mca_result <- mca(country)
  
  # Get cumulative variance
  cumulative_variance <- mca_result$eig$variance.cumulative
  
  # Calculate the percentage of variability explained by the first 5 dimensions
  # Check if we have at least 5 dimensions
  if(length(cumulative_variance) >= 5) {
    percentage_explained <- cumulative_variance[5] * 100
  } else {
    # If fewer than 5 dimensions, use the last available one
    percentage_explained <- cumulative_variance[length(cumulative_variance)] * 100
  }
  
  return(percentage_explained)
}

#' Create Visualization Plots for MCA Results
#'
#' This function generates visualization plots for MCA results, including
#' a scree plot and contribution plots for the first few dimensions.
#'
#' @param country Character string indicating the country to analyze
#' @return A combined plot object (using patchwork) with scree and contribution plots
MCA_plot <- function(country){
  mca_result <- mca(country)
  
  # Create scree plot
  scree_plot <- fviz_screeplot(mca_result, addlabels = TRUE, ylim = c(0, 90))
  
  # Create contribution plots for available dimensions (up to 3)
  n_dims <- ncol(mca_result$var$contrib)
  dim_plots <- list()
  for(i in 1:min(3, n_dims)) {
    dim_plots[[i]] <- fviz_contrib(mca_result, choice = "var", axes = i, top = 15)
  }
  
  # If we don't have all 3 dimensions, pad with NULL
  while(length(dim_plots) < 3) {
    dim_plots[[length(dim_plots) + 1]] <- NULL
  }
  
  # Combine available plots
  if(all(sapply(dim_plots, is.null))) {
    # If all contribution plots are NULL, just return the scree plot
    combined_plot <- scree_plot
  } else {
    # Filter out NULL plots
    valid_plots <- dim_plots[!sapply(dim_plots, is.null)]
    combined_plot <- scree_plot
    
    for(plot in valid_plots) {
      combined_plot <- combined_plot + plot
    }
    combined_plot <- combined_plot + plot_layout(ncol = 2, nrow = 2)
  }
  
  return(combined_plot)
}

#==============================================================================
# VISUALIZATION DEPENDENT-INDEPENDENT VARIABLES
#==============================================================================

#' Plot Beta Coefficients and MCA Factors Together
#'
#' This function creates a combined visualization showing both the estimated beta
#' coefficients (dependent variables) and the MCA factors (independent variables)
#' for a given country, allowing visual examination of their relationships.
#'
#' @param country Character string indicating the country to analyze
#' @param percentile Character string indicating which percentile to use ("50", "2.5", or "97.5")
#' @return A combined ggplot object with two plots arranged vertically
estBeta_mcaNPI <- function(country,percentile) {
  # Combine beta coefficients with MCA factors, filtering to matching date range
  beta_df <- fitted_beta_coefficients(country, percentile)
  mca_df <- mca_factors(country)
  
  # Get date ranges from comix data
  min_date <- min(comix_dates(country)[,1])
  max_date <- max(comix_dates(country)[,1])
  
  # Filter mca_factors properly
  filtered_mca <- mca_df %>%
    filter(date >= min_date) %>%
    filter(date <= max_date)
  
  # Combine the data frames
  df <- cbind(beta_df, filtered_mca[,-1])
  
  long_data <- pivot_longer(df, cols = -Date, names_to = "variable", values_to = "value")
  
  subset_top <- long_data %>% filter(variable %in% c("Date", "beta1", "beta2", "beta3", "beta4",
                                                     "beta1_w", "beta2_w", "beta3_w", "beta4_w"))
  # Create the ggplot for the top plot
  plot_top <- ggplot(subset_top, aes(x = Date, y = value, color = variable)) +
    geom_line(data = subset(subset_top, variable %in% c("beta1", "beta2", "beta3", "beta4")), show.legend = TRUE) +
    geom_point(data = subset(subset_top, variable %in% c("beta1_w", "beta2_w", "beta3_w", "beta4_w")), show.legend = TRUE) +
    labs(x = "Time", y = "Median estimated change in contacts per setting", title = "Dependent") +
    scale_color_discrete(name = "Time-varying Coefficients", 
                         breaks = c("beta1", "beta2", "beta3", "beta4"))+
    scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  # Subset the data for the second ggplot (bottom plot)
  subset_bottom <- long_data %>% filter(variable %in% c("Date", "Dim 1", "Dim 2", "Dim 3", "Dim 4","Dim 5"))
  
  # Create the ggplot for the bottom plot
  plot_bottom <- ggplot(subset_bottom, aes(x = Date, y = value, color = variable)) +
    geom_line() +
    labs(x = "time", y = "MCA factors", title = "Independent") +
    scale_color_discrete(name = "Factors")+
    scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  combined_plot <- plot_top + plot_bottom + plot_layout(ncol = 1)
  
  return(combined_plot)
}


#==============================================================================
# DATA PREPARATION FOR STAN MODELING
#==============================================================================

#' Prepare Data for Stan Regression Model
#'
#' This function combines extracted data from MCA analysis and beta coefficients
#' into a list structure suitable for Stan modeling.
#'
#' @param country Character string indicating the country to analyze
#' @param percentile Character string indicating which percentile to use ("50", "2.5", or "97.5")
#' @return A list containing all data needed for Stan modeling
reg_data_to_stan_list <- function(country, percentile){
  
  mca_df <- mca_factors(country)
  min_date <- min(comix_dates(country)[,1])
  max_date <- max(comix_dates(country)[,1])
  filtered_mca <- mca_df %>%
    filter(date >= min_date) %>%
    filter(date <= max_date)
  
  reg_data = list(n_train = nrow(filtered_mca[,-1]),
                  n_pred = nrow(mca_df[,-1]),
                  K = ncol(fitted_beta_coefficients(country,percentile)[,-c(1,2,4,6,8)]),  #number of regressions
                  J = ncol(mca_df)-1,  #number of factor dimensions
                  I = ncol(contact_data(country)[[2]][,-1]),
                  x_train = filtered_mca[,-1],
                  x_pred = mca_df[,-1],
                  beta_train = fitted_beta_coefficients(country,percentile)[,-c(1,2,4,6,8)],
                  beta0_train = fitted_interc(country,percentile),
                  X = t(as.matrix(contact_data(country)[[1]][,])))
  
  return(reg_data)
}
