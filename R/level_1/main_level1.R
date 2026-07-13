# =============================================================================
# main_level1.R
# 
# This script implements Level 1 of the contact pattern modeling framework,
# fitting Time-Varying Effect Models (TVEM) to estimate the temporal changes
# in social contact patterns during the COVID-19 pandemic for countries with 
# CoMix data.
# =============================================================================

# ---- Load required libraries ----
library(Bernadette)     
library(rstan)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# ---- Configure Stan options ----
rstan_options(auto_write = TRUE)            
options(mc.cores = parallel::detectCores()) 
set.seed(1234)  

# ---- Load source files for data processing, model fitting, and figure generation ----
source("R/level_1/contact_data_new.R")      # Functions for loading and processing contact data
source("R/level_1/fit_level1_new.R")        # Functions for fitting Stan models
source("R/level_1/figures_level1_new.R")    # Functions for generating figures from model outputs
source("R/level_2/comix_waves_dates.R")     # Functions for handling CoMix survey waves/dates
source("R/level_2/regression_data_new.R")       # Functions for preparing regression data

# ==============================================================================
# AGE GROUP CONFIGURATION
# ==============================================================================

# Age group configuration - MODIFY THESE TO CHANGE THE NUMBER OF AGE GROUPS
n_age_groups <- 6
age_group_labels <- c("[0-10)", "[10,20)", "[20,25)", "[25,45)", "[45,65)", "65+")

# Age group mapping for POLYMOD aggregation (from 16 original groups to target groups)
# Original POLYMOD groups: 0-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75+
age_group_mapping <- c(
  rep(age_group_labels[1], 2),    # 0-4, 5-9 -> [0-10)
  rep(age_group_labels[2], 2),    # 10-14, 15-19 -> [10,20)
  rep(age_group_labels[3], 1),    # 20-24  -> [20,25)
  rep(age_group_labels[4], 4),    # 25-29, 30-34, 35-39, 40-44  -> [25,45)
  rep(age_group_labels[5], 4),    # 45-49, 50-54, 55-59, 60-64  -> [45,65)
  rep(age_group_labels[6], 3)     # 65-69, 70-74, 75+ -> 65+
)

# Validate configuration
if (length(age_group_mapping) != 16) {
  stop("age_group_mapping must have exactly 16 elements (one for each original POLYMOD age group)")
}

if (length(age_group_labels) != n_age_groups) {
  stop("Length of age_group_labels must match n_age_groups")
}

cat("=============================================================================\n")
cat("AGE GROUP CONFIGURATION:\n")
cat("Number of age groups:", n_age_groups, "\n")
cat("Age group labels:", paste(age_group_labels, collapse = ", "), "\n")
cat("=============================================================================\n")

# ---- Define list of CoMix countries to analyze ----
# These are countries with available CoMix contact survey data
countries <- c("AT", "BE", "HR", "DK", "EE", "FI", "FR",
               "GR", "HU", "IT", "LT", "NL", "PL", "PT",
               "SK", "SI", "ES", "UK")

# ---- Set MCMC sampling parameters ----
n_chains = 4       
n_warmups = 1000   
n_iter = 4000     
n_thin = 1          
n_adapt_delta = 0.95 
n_max_treedepth = 10 

# ---- B-spline parameters ----
n_knots = 10             # Number of knots for B-spline basis (controls flexibility)
#n_knots = 50            # Alternative with more knots (more flexible but risk of overfitting)
s_degree = 3             # Degree of the B-spline basis (cubic splines)

# ---- Path to Stan model file ----
#model1 = file.path("stan_models", "m1_penalised.stan")   # penalized model NO intercept
model2 = file.path("stan_models", "m2_penalised.stan")    # penalized model Fixed pair-specific intercept, derived as the time-average of the per-wave projection residuals

# ---- Model hyperparameters ----
n_aa <- 1 
n_tt1 <- 5 
n_tt2 <- 5
n_tt3 <- 5
n_tt4 <- 5
n_ss <- 5

# ---- Specify parameters to monitor in Stan output ----
parameters = c("beta1", "beta2", "beta3", "beta4",
               "tau1", "tau2", "tau3", "tau4",
               "sigma",
               "y_pred", "log_lik", "dev")

# =============================================================================
# MODEL FITTING SECTION
# =============================================================================

# ---- Fit the model and save results for each country ----
cat("\nFitting Time-Varying Effect Models for each country...\n")
cat("Using", n_age_groups, "age groups:", paste(age_group_labels, collapse = ", "), "\n\n")

for (country in countries) {
  cat(paste0("Processing country: ", country, "\n"))
  
  # Check if model fit already exists
  output_file <- paste0('outputs_level1/', country, '_TVEM_', n_age_groups, 'groups.RData')
  if (file.exists(output_file)) {
    cat(paste0("  Model fit already exists for ", country, ", skipping...\n"))
    next
  }
  
  # Fit the Stan model using the nuts_fit function from fit_level1.R
  # This estimates time-varying coefficients (beta) for each contact setting
  cat(paste0("  Fitting TVEM model for ", country, " with ", n_age_groups, " age groups...\n"))
  
  tryCatch({
    fit <- nuts_fit(model2, country, n_chains, n_warmups, n_iter, n_thin, n_adapt_delta, n_max_treedepth,
                    age_group_labels = age_group_labels, 
                    age_group_mapping = age_group_mapping)
    
    # Save the entire workspace including the fitted model
    # This allows for later analysis without re-running the model
    save.image(file = output_file)
    cat(paste0("  Model fit saved for ", country, "\n"))
    
  }, error = function(e) {
    cat(paste0("  ERROR fitting model for ", country, ": ", e$message, "\n"))
  })
}

# =============================================================================
# VISUALIZATION SECTION
# =============================================================================

# ---- Generate and save beta parameter plots for each country ----
cat("\nGenerating beta coefficient plots for each country...\n")

for (country in countries) {
  cat(paste0("Creating beta plots for: ", country, "\n"))
  
  tryCatch({
    # Create plot of beta coefficients over time using fitted_beta function
    # This shows how contact patterns for different settings (home, work, school, other)
    # changed throughout the pandemic period
    fig1 <- fitted_beta(country, n_age_groups= n_age_groups)
    fig1 <- fitted_beta_coefficients <- function(country, percentile, n_age_groups)
    # Save the plot if it was created successfully
    if (!is.null(fig1)) {
      output_file <- paste0('figures/fit_beta/', country, '_beta_', n_age_groups, 'groups.png')
      
      # Create directory if it doesn't exist
      dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
      
      png(filename = output_file, width = 1000, height = 800)
      print(fig1)
      dev.off()
      cat(paste0("  Beta plot saved to: ", output_file, "\n"))
    } else {
      cat(paste0("  Warning: Could not generate beta plot for ", country, "\n"))
    }
    
  }, error = function(e) {
    cat(paste0("  ERROR creating beta plot for ", country, ": ", e$message, "\n"))
  })
}

# ---- Generate and save interpolated beta parameter plots for each country ----
cat("\nGenerating interpolated beta coefficient plots for each country...\n")

for (country in countries) {
  cat(paste0("Creating interpolated beta plots for: ", country, "\n"))
  
  tryCatch({
    # Create plot of beta coefficients over time using fitted_beta_interp_all function
    # This shows how contact patterns for different settings (home, work, school, other)
    # changed throughout the pandemic period with interpolation
    fig2 <- fitted_beta_interp_all(country, n_age_groups= n_age_groups)
    
    # Save the plot if it was created successfully
    if (!is.null(fig2)) {
      output_file <- paste0('figures/fit_beta_interpolated/', country, '_beta_int_', n_age_groups, 'groups.png')
      
      # Create directory if it doesn't exist
      dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
      
      png(filename = output_file, width = 1000, height = 800)
      print(fig2)
      dev.off()
      cat(paste0("  Interpolated beta plot saved to: ", output_file, "\n"))
    } else {
      cat(paste0("  Warning: Could not generate interpolated beta plot for ", country, "\n"))
    }
    
  }, error = function(e) {
    cat(paste0("  ERROR creating interpolated beta plot for ", country, ": ", e$message, "\n"))
  })
}

# ---- Generate and save contact pattern plots for each country ----
cat("\nGenerating contact pattern plots for each country...\n")

for (country in countries) {
  cat(paste0("Creating contact pattern plots for: ", country, "\n"))
  
  tryCatch({
    # Create plot of contact patterns over time using fitted_contacts function
    # This shows more detailed contact rates between different age groups
    # and how they changed throughout the pandemic
    fig3 <- fitted_contacts(country, age_group_labels = age_group_labels, n_age_groups=n_age_groups)
    
    # Save the plot if it was created successfully
    if (!is.null(fig3)) {
      output_file <- paste0('figures/fit_C/', country, '_contacts_', n_age_groups, 'groups.png')
      
      # Create directory if it doesn't exist
      dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
      
      # Adjust plot size based on number of age groups
      plot_size <- max(1200, n_age_groups * 200)
      
      png(filename = output_file, width = plot_size, height = plot_size)
      print(fig3)
      dev.off()
      cat(paste0("  Contact pattern plot saved to: ", output_file, "\n"))
    } else {
      cat(paste0("  Warning: Could not generate contact pattern plot for ", country, "\n"))
    }
    
  }, error = function(e) {
    cat(paste0("  ERROR creating contact pattern plot for ", country, ": ", e$message, "\n"))
  })
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("Level 1 model fitting and visualization completed.\n")
cat("Configuration used:\n")
cat("- Number of age groups:", n_age_groups, "\n")
cat("- Age group labels:", paste(age_group_labels, collapse = ", "), "\n")
cat("- Countries processed:", length(countries), "\n")
cat("- Output files saved with suffix: '_", n_age_groups, "groups'\n")
cat("=============================================================================\n")