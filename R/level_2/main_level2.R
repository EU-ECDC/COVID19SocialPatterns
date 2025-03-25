# =============================================================================
# main_level2.R
# 
# This script implements Level 2 of the contact pattern modeling framework,
# fitting multivariate regression models to link Non-Pharmaceutical Interventions (NPIs)
# to the time-varying contact coefficients estimated in Level 1.
# It also performs clustering and matching to extend predictions to countries
# without CoMix survey data.
# =============================================================================

# ---- Load required libraries ----
library(rstan)        
library(readr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggcorrplot) 
library(patchwork)
library(MASS) 
library(reshape2) 
library(viridis)
library(gridExtra) 
library(stringr) 
library(cluster) 
library(fossil)

# ---- Configure Stan options ----
rstan_options(auto_write = TRUE)           
options(mc.cores = parallel::detectCores()) 
set.seed(1234)                         

# ---- Load source files for data processing, model fitting, and figure generation ----
source("R/level_1/contact_data.R")          # Functions for loading contact data
source("R/level_1/fit_level1.R")            # Functions for accessing Level 1 model fits
source("R/level_2/NPI_data.R")              # Functions for loading NPI data
source("R/level_2/comix_waves_dates.R")     # Functions for handling CoMix survey waves/dates
source("R/level_2/regression_data.R")       # Functions for preparing regression data
source("R/level_2/fit_level2.R")            # Functions for fitting MVR models
source("R/level_2/save_matrices.R")         # Functions for saving contact matrices as CSV files
source("R/level_2/figures_level2.R")        # Functions for creating Level 2 visualizations
source("R/level_2/matching.R")              # Functions for matching non-CoMix to CoMix countries
source("R/level_2/clustering.R")            # Functions for clustering countries by NPI patterns
source("R/level_2/noComix_C.R")             # Functions for constructing contacts for non-CoMix countries

# ---- Define list of CoMix countries to analyze ----
# These are countries with available CoMix contact survey data
countries <- c("AT", "BE", "HR", "DK", "EE", "FI", "FR", "GR", "HU",
               "IT", "LT", "NL", "PL", "PT", "SK", "SI", "ES")

# ---- Define percentiles for uncertainty quantification ----
# These represent median and 95% confidence interval bounds
percentiles <- c("50", "2.5", "97.5")

# ---- Set MCMC sampling parameters ----
n_chains = 4         
n_warmups = 1000 
n_iter = 4000
n_thin = 1   
n_adapt_delta = 0.95  
n_max_treedepth = 10 

# ---- Specify parameters to monitor in Stan output ----
parameters = c("delta0", "delta", "L_Omega", "L_sigma", 
               "mu", "L_Sigma",                        
               "log_lik", "dev", "Sigma", "Omega",  
               "mu_pred", "beta_pred", "C_pred") 

# =============================================================================
# MULTIPLE CORRESPONDENCE ANALYSIS (MCA) OF NPI DATA
# =============================================================================

cat("\nPerforming Multiple Correspondence Analysis of NPI data...\n")

# Generate MCA plots to visualize NPI patterns for each country
# MCA_plot function performs multiple correspondence analysis on NPI data
# and creates plots showing the main dimensions of variation
for (country in countries) {
  cat(paste0("Creating MCA plot for: ", country, "\n"))
  
  mca_fig <- MCA_plot(country)
  if (!is.null(mca_fig)) {
    output_file <- paste0('figures/mca_plots/', country, '_mca.png')
    png(filename = output_file, width = 1000, height = 1000)
    print(mca_fig)
    dev.off()
    cat(paste0("  MCA plot saved to: ", output_file, "\n"))
  } else {
    cat(paste0("  Warning: Could not generate MCA plot for ", country, "\n"))
  }
}

# =============================================================================
# VISUALIZATION OF REGRESSION DATA (DEPENDENT & INDEPENDENT VARIABLES)
# =============================================================================

cat("\nGenerating visualizations of regression data (dependent & independent variables)...\n")

# Create plots showing both the dependent variables (beta coefficients from Level 1)
# and independent variables (MCA factors from NPI analysis) together
# This helps visualize potential relationships between NPIs and contact patterns
for (country in countries) {
  for (percentile in percentiles) {
    cat(paste0("Creating regression data visualization for: ", country, " (", percentile, "%)...\n"))
    
    fig1 <- estBeta_mcaNPI(country, percentile)
    if (!is.null(fig1)) {
      output_file <- paste0('figures/reg_data/', country, '_', percentile, '_dep_indep.png')
      png(filename = output_file, width = 900, height = 600)
      print(fig1)
      dev.off()
      cat(paste0("  Regression data plot saved to: ", output_file, "\n"))
    } else {
      cat(paste0("  Warning: Could not generate regression data plot for ", country, " (", percentile, "%)\n"))
    }
  }
}

# =============================================================================
# MULTIVARIATE REGRESSION MODEL
# =============================================================================

cat("\nFitting multivariate regression models...\n")

# Fit multivariate regression (MVR) models to connect NPI factors to estimated beta coefficients
# for each country and percentile level (median and 95% CI bounds)
for (country in countries) {
  for (percentile in percentiles) {
    cat(paste0("Fitting MVR model for: ", country, " (", percentile, "%)...\n"))
    
    # Check if model fit already exists
    output_file <- paste0('outputs_level2/', country, '_', percentile, '_MVR.RData')
    if (file.exists(output_file)) {
      cat(paste0("  Model fit already exists for ", country, " (", percentile, "%), skipping...\n"))
      next
    }
    
    # Fit the multivariate regression model using the nuts_reg function
    # This links NPI factors (from MCA) to contact coefficients (from Level 1)
    fit <- nuts_reg(country, percentile, n_chains, n_warmups, n_iter, n_thin, n_adapt_delta, n_max_treedepth)
    
    # Save the entire workspace including the fitted model
    save.image(file = output_file)
    cat(paste0("  Model fit saved to: ", output_file, "\n"))
  }
}

# =============================================================================
# VISUALIZATION OF FITTED BETA COEFFICIENTS
# =============================================================================

cat("\nGenerating visualizations of fitted beta coefficients...\n")

# Create plots showing the predicted beta coefficients from the MVR model
# compared to the beta coefficients from Level 1
for (country in countries) {
  for (percentile in percentiles) {
    cat(paste0("Creating beta coefficient plots for: ", country, " (", percentile, "%)...\n"))
    
    fig2 <- fitted_median_beta(country, percentile)
    if (!is.null(fig2)) {
      output_file <- paste0('figures/fit_beta_NPI/', country, '_', percentile, '_beta.png')
      png(filename = output_file, width = 2500, height = 1875)
      print(fig2)
      dev.off()
      cat(paste0("  Beta coefficient plot saved to: ", output_file, "\n"))
    } else {
      cat(paste0("  Warning: Could not generate beta coefficient plot for ", country, " (", percentile, "%)\n"))
    }
  }
}

# =============================================================================
# SAVE AND VISUALIZE CONTACT MATRICES
# =============================================================================

cat("\nSaving contact matrices and creating heatmaps...\n")

# Save contact matrices (based on the posterior median from level 1) to CSV files for later use
for (country in countries) {
  cat(paste0("Saving median contact matrices for: ", country, "\n"))
  save_contact_matrices(country, "50")
}

# Create heatmap visualizations of the contact matrices
for (country in countries) {
  cat(paste0("Creating contact matrix heatmaps for: ", country, "\n"))
  create_contact_matrix_heatmaps(country, "50")
}

# Generate plots showing the predicted contact patterns 
# (based on the posterior median from level 1) over time 
# compared to the observed CoMix data
for (country in countries) {
  cat(paste0("Creating contact pattern plots for: ", country, "\n"))
  
  fig3 <- pred_CM(country, "50")
  if (!is.null(fig3)) {
    output_file <- paste0('figures/fit_C_NPI/', country, '_contacts.png')
    png(filename = output_file, width = 3000, height = 1875)
    print(fig3)
    dev.off()
    cat(paste0("  Contact pattern plot saved to: ", output_file, "\n"))
  } else {
    cat(paste0("  Warning: Could not generate contact pattern plot for ", country, "\n"))
  }
}

# Create plots showing the full prediction uncertainty (using all percentiles)
for (country in countries) {
  cat(paste0("Creating full contact pattern plots for: ", country, "\n"))
  
  fig4 <- pred_CM_full(country)
  if (!is.null(fig4)) {
    output_file <- paste0('figures/fit_C_NPI/', country, '_contacts_full.png')
    png(filename = output_file, width = 3000, height = 1875)
    print(fig4)
    dev.off()
    cat(paste0("  Full contact pattern plot saved to: ", output_file, "\n"))
  } else {
    cat(paste0("  Warning: Could not generate full contact pattern plot for ", country, "\n"))
  }
}

# =============================================================================
# CLUSTERING AND MATCHING ANALYSIS FOR NON-COMIX COUNTRIES
# =============================================================================

cat("\n=============================================================================\n")
cat("Performing clustering and matching analysis for non-CoMix countries...\n")

# Define countries with and without CoMix data
comix_countries <- c("AT", "BE", "HR", "DK", "EE", "FI", "FR", "GR", "HU",
                     "IT", "LT", "NL", "PL", "PT", "SK", "SI", "ES")
non_comix_countries <- c("CZ", "BG", "CY", "IE", "LV", "LU", "RO",
                         "SE", "MT", "IS", "DE")

# Perform hierarchical clustering of countries based on NPI patterns
# This identifies groups of countries with similar intervention strategies
cat("\nClustering countries by NPI patterns...\n")
clustering_result <- cluster_countries(c(comix_countries, non_comix_countries))

# Match non-CoMix countries to similar CoMix countries
# This helps identify which CoMix country's data can best represent each non-CoMix country
cat("\nMatching non-CoMix countries to similar CoMix countries...\n")
match_results <- match_countries(comix_countries, non_comix_countries,
                                 top_n = 3,
                                 cluster_results = clustering_result)

# Visualize the matching results to show which countries are most similar
cat("\nVisualizing country matching results...\n")
match_df <- visualize_matches(match_results, plot_type = "both", show_top_n = 2)

# Define country pairs for prediction based on clustering/matching results
# Each pair consists of: [CoMix country, non-CoMix country]
cat("\nDefining country pairs for prediction...\n")
country_pairs <- list(
  c("IT", "CZ"),     # Italy -> Czech Republic
  c("HR", "BG"),     # Croatia -> Bulgaria
  c("IT", "CY"),     # Italy -> Cyprus
  c("FR", "IE"),     # France -> Ireland
  c("ES", "LV"),     # Spain -> Latvia
  c("DK", "LU"),     # Denmark -> Luxembourg
  c("HR", "RO"),     # Croatia -> Romania
  c("FR", "SE"),     # France -> Sweden
  c("IT", "MT"),     # Italy -> Malta
  c("HR", "IS"),     # Croatia -> Iceland
  c("FR", "DE")      # France -> Germany
)

# =============================================================================
# GENERATE PREDICTIONS FOR NON-COMIX COUNTRIES
# =============================================================================

cat("\nGenerating contact matrix predictions for non-CoMix countries...\n")

# For each country pair, generate contact matrices for the non-CoMix country
for (pair in country_pairs) {
  country_1 <- pair[1]  # CoMix country (source)
  country_2 <- pair[2]  # Non-CoMix country (target)
  
  cat(sprintf("Generating predictions: %s -> %s\n", country_1, country_2))
  generate_CM_predictions(country_1, country_2, save_matrices = TRUE)
}

# Create visualizations of the predicted contact matrices
cat("\nCreating visualization plots for non-CoMix country predictions...\n")
for (pair in country_pairs) {
  country_1 <- pair[1]  # CoMix country (source)
  country_2 <- pair[2]  # Non-CoMix country (target)
  
  csv_path <- paste0("outputs_level2/CM/", country_2, "_from_", country_1, "_contact_matrices.csv")
  
  cat(sprintf("Creating figure for: %s -> %s\n", country_1, country_2))
  fig5 <- try(plot_CM_from_csv(csv_path), silent = TRUE)
  
  if (!inherits(fig5, "try-error") && !is.null(fig5)) {
    dir.create("figures/fit_C_NPI/noCoMix/", recursive = TRUE, showWarnings = FALSE)
    output_file <- paste0("figures/fit_C_NPI/noCoMix/", country_2, "_from_", country_1, "_contacts.png")
    png(filename = output_file, width = 3000, height = 1875)
    print(fig5)
    dev.off()
    cat(sprintf("  Figure saved to: %s\n", output_file))
  } else {
    cat(sprintf("  ERROR: Could not create figure for %s -> %s\n", country_1, country_2))
  }
}

cat("\nLevel 2 analysis completed.\n")