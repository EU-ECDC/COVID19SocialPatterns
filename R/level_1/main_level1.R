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
source("R/level_1/contact_data.R")      # Functions for loading and processing contact data
source("R/level_1/fit_level1.R")        # Functions for fitting Stan models
source("R/level_1/figures_level1.R")    # Functions for generating figures from model outputs

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
#model1 = file.path("stan_models", "m1_penalised_test.stan")  # Alternative model (not used)
model2 = file.path("stan_models", "m2_penalised.stan")        # Main penalized model

# ---- Model hyperparameters ----
n_aa <- 1  
n_tt1 <- 5 
n_tt2 <- 5
n_tt3 <- 5
n_tt4 <- 5
n_ss <- 5

# ---- Specify parameters to monitor in Stan output ----
# Alternative parameterization without intercept
# parameters = c("beta1", "beta2", "beta3", "beta4", 
#                "tau1", "tau2", "tau3", "tau4", 
#                "sigma",
#                "y_pred", "log_lik", "dev")

# Current parameterization with intercept term
parameters = c("beta0", "beta1", "beta2", "beta3", "beta4", 
               "tau1", "tau2", "tau3", "tau4", 
               "sigma",
               "y_pred", "log_lik", "dev")

# =============================================================================
# MODEL FITTING SECTION
# =============================================================================

# ---- Fit the model and save results for each country ----
cat("\nFitting Time-Varying Effect Models for each country...\n")

for (country in countries) {
  cat(paste0("Processing country: ", country, "\n"))
  
  # Check if model fit already exists
  output_file <- paste0('outputs_level1/', country, '_TVEM.RData')
  if (file.exists(output_file)) {
    cat(paste0("  Model fit already exists for ", country, ", skipping...\n"))
    next
  }
  
  # Fit the Stan model using the nuts_fit function from fit_level1.R
  # This estimates time-varying coefficients (beta) for each contact setting
  cat(paste0("  Fitting TVEM model for ", country, "...\n"))
  fit <- nuts_fit(model2, country, n_chains, n_warmups, n_iter, n_thin, n_adapt_delta, n_max_treedepth)
  
  # Save the entire workspace including the fitted model
  # This allows for later analysis without re-running the model
  save.image(file = output_file)
  cat(paste0("  Model fit saved for ", country, "\n"))
}

# =============================================================================
# VISUALIZATION SECTION
# =============================================================================

# ---- Generate and save beta parameter plots for each country ----cat("\nGenerating beta coefficient plots for each country...\n")

for (country in countries) {
  cat(paste0("Creating beta plots for: ", country, "\n"))
  
  # Create plot of beta coefficients over time using fitted_beta function
  # This shows how contact patterns for different settings (home, work, school, other)
  # changed throughout the pandemic period
  fig1 <- fitted_beta(country)
  
  # Save the plot if it was created successfully
  if (!is.null(fig1)) {
    output_file <- paste0('figures/fit_beta/', country, '_beta.png')
    png(filename = output_file, width = 1000, height = 800)
    print(fig1)
    dev.off()
    cat(paste0("  Beta plot saved to: ", output_file, "\n"))
  } else {
    cat(paste0("  Warning: Could not generate beta plot for ", country, "\n"))
  }
}

# ---- Generate and save contact pattern plots for each country ----
cat("\nGenerating contact pattern plots for each country...\n")

for (country in countries) {
  cat(paste0("Creating contact pattern plots for: ", country, "\n"))
  
  # Create plot of contact patterns over time using fitted_contacts function
  # This shows more detailed contact rates between different age groups
  # and how they changed throughout the pandemic
  fig2 <- fitted_contacts(country)
  
  # Save the plot if it was created successfully
  if (!is.null(fig2)) {
    output_file <- paste0('figures/fit_C/', country, '_contacts.png')
    png(filename = output_file, width = 1875, height = 1875)
    print(fig2)
    dev.off()
    cat(paste0("  Contact pattern plot saved to: ", output_file, "\n"))
  } else {
    cat(paste0("  Warning: Could not generate contact pattern plot for ", country, "\n"))
  }
}

cat("\nLevel 1 model fitting and visualization completed.\n")