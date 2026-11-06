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
source("R/level_1/contact_data_new.R")      # Functions for loading contact data
source("R/level_1/fit_level1_new.R")        # Functions for accessing Level 1 model fits
source("R/level_1/figures_level1_new.R")        # Functions for creating Level 1 visualizations
source("R/level_2/NPI_data.R")              # Functions for loading NPI data
source("R/level_2/comix_waves_dates.R")     # Functions for handling CoMix survey waves/dates
source("R/level_2/regression_data_new.R")   # Functions for preparing regression data
source("R/level_2/fit_level2_new.R")        # Functions for fitting MVR models
source("R/level_2/save_matrices_new.R")     # Functions for saving contact matrices as CSV files
source("R/level_2/figures_level2_new.R")        # Functions for creating Level 2 visualizations
source("R/level_2/matching.R")              # Functions for matching non-CoMix to CoMix countries
source("R/level_2/clustering.R")            # Functions for clustering countries by NPI patterns
source("R/level_2/noComix_C_new.R")             # Functions for constructing contacts for non-CoMix countries

# ==============================================================================
# AGE GROUP CONFIGURATION
# ==============================================================================

# Age group configuration - MODIFY THESE TO CHANGE THE NUMBER OF AGE GROUPS
n_age_groups <- 6
age_group_labels <- c("[0-10)", "[10,20)", "[20,25)", "[25,45)", "[45,65)", "65+")

cat("=============================================================================\n")
cat("AGE GROUP CONFIGURATION:\n")
cat("Number of age groups:", n_age_groups, "\n")
cat("Age group labels:", paste(age_group_labels, collapse = ", "), "\n")
cat("=============================================================================\n")

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
cat("Using", n_age_groups, "age groups:", paste(age_group_labels, collapse = ", "), "\n\n")

# Generate MCA plots to visualize NPI patterns for each country
# MCA_plot function performs multiple correspondence analysis on NPI data
# and creates plots showing the main dimensions of variation
for (country in countries) {
  cat(paste0("Creating MCA plot for: ", country, "\n"))
  
  tryCatch({
    mca_fig <- MCA_plot(country)
    if (!is.null(mca_fig)) {
      output_file <- paste0('figures/mca_plots/', country, '_mca.png')
      
      # Create directory if it doesn't exist
      dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
      
      png(filename = output_file, width = 1000, height = 1000)
      print(mca_fig)
      dev.off()
      cat(paste0("  MCA plot saved to: ", output_file, "\n"))
    } else {
      cat(paste0("  Warning: Could not generate MCA plot for ", country, "\n"))
    }
  }, error = function(e) {
    cat(paste0("  ERROR creating MCA plot for ", country, ": ", e$message, "\n"))
  })
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
    
    tryCatch({
      fig1 <- estBeta_mcaNPI(country, percentile, n_age_groups)
      if (!is.null(fig1)) {
        output_file <- paste0('figures/reg_data/', country, '_', percentile, '_', n_age_groups, 'groups_dep_indep.png')
        
        # Create directory if it doesn't exist
        dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
        
        png(filename = output_file, width = 900, height = 600)
        print(fig1)
        dev.off()
        cat(paste0("  Regression data plot saved to: ", output_file, "\n"))
      } else {
        cat(paste0("  Warning: Could not generate regression data plot for ", country, " (", percentile, "%)\n"))
      }
    }, error = function(e) {
      cat(paste0("  ERROR creating regression data plot for ", country, " (", percentile, "%): ", e$message, "\n"))
    })
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
    output_file <- paste0('outputs_level2/', country, '_', percentile, '_MVR_', n_age_groups, 'groups.RData')
    if (file.exists(output_file)) {
      cat(paste0("  Model fit already exists for ", country, " (", percentile, "%), skipping...\n"))
      next
    }
    
    # Fit the multivariate regression model using the nuts_reg function
    # This links NPI factors (from MCA) to contact coefficients (from Level 1)
    tryCatch({
      fit <- nuts_reg(country, percentile, n_chains, n_warmups, n_iter, n_thin, n_adapt_delta, n_max_treedepth, n_age_groups)
      
      # Save the entire workspace including the fitted model
      save.image(file = output_file)
      cat(paste0("  Model fit saved to: ", output_file, "\n"))
    }, error = function(e) {
      cat(paste0("  ERROR fitting MVR model for ", country, " (", percentile, "%): ", e$message, "\n"))
    })
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
    
    tryCatch({
      fig2 <- fitted_median_beta(country, percentile, n_age_groups)
      if (!is.null(fig2)) {
        output_file <- paste0('figures/fit_beta_NPI/', country, '_', percentile, '_', n_age_groups, 'groups_beta.png')
        
        # Create directory if it doesn't exist
        dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
        
        png(filename = output_file, width = 2500, height = 1875)
        print(fig2)
        dev.off()
        cat(paste0("  Beta coefficient plot saved to: ", output_file, "\n"))
      } else {
        cat(paste0("  Warning: Could not generate beta coefficient plot for ", country, " (", percentile, "%)\n"))
      }
    }, error = function(e) {
      cat(paste0("  ERROR creating beta coefficient plot for ", country, " (", percentile, "%): ", e$message, "\n"))
    })
  }
}

# =============================================================================
# SAVE AND VISUALIZE CONTACT MATRICES
# =============================================================================

cat("\nSaving contact matrices and creating heatmaps...\n")

# Save contact matrices (based on the posterior median from level 1) to CSV files for later use
for (country in countries) {
  for (percentile in percentiles) {
    cat(paste0("Saving contact matrices for: ", country, " (", percentile, "%)...\n"))
    tryCatch({
      save_contact_matrices(country, percentile, n_age_groups, age_group_labels)
      cat(paste0("  Contact matrices saved for ", country, " (", percentile, "%)\n"))
    }, error = function(e) {
      cat(paste0("  ERROR saving contact matrices for ", country, " (", percentile, "%): ", e$message, "\n"))
    })
  }
}

# Create heatmap visualizations of the contact matrices
for (country in countries) {
  for (percentile in percentiles) {
    cat(paste0("Creating contact matrix heatmaps for: ", country, " (", percentile, "%)...\n"))
    tryCatch({
      create_contact_matrix_heatmaps(country, percentile, n_age_groups, age_group_labels)
      cat(paste0("  Heatmaps created for ", country, " (", percentile, "%)\n"))
    }, error = function(e) {
      cat(paste0("  ERROR creating heatmaps for ", country, " (", percentile, "%): ", e$message, "\n"))
    })
  }
}

# Generate plots showing the predicted contact patterns 
# (based on the posterior median from level 1) over time 
# compared to the observed CoMix data
for (country in countries) {
  cat(paste0("Creating contact pattern plots for: ", country, "\n"))
  
  tryCatch({
    fig3 <- pred_CM(country, "50", n_age_groups, age_group_labels)
    if (!is.null(fig3)) {
      output_file <- paste0('figures/fit_C_NPI/', country, '_', n_age_groups, 'groups_contacts.png')
      
      # Create directory if it doesn't exist
      dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
      
      # Adjust plot size based on number of age groups
      plot_size <- max(3000, n_age_groups * 500)
      
      png(filename = output_file, width = plot_size, height = 1875)
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

# Create plots showing the full prediction uncertainty (using all percentiles)
for (country in countries) {
  cat(paste0("Creating full contact pattern plots for: ", country, "\n"))
  
  tryCatch({
    fig4 <- pred_CM_full(country, n_age_groups, age_group_labels)
    if (!is.null(fig4)) {
      output_file <- paste0('figures/fit_C_NPI/', country, '_', n_age_groups, 'groups_contacts_full.png')
      
      # Create directory if it doesn't exist
      dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
      
      # Adjust plot size based on number of age groups
      plot_size <- max(3000, n_age_groups * 500)
      
      png(filename = output_file, width = plot_size, height = 1875)
      print(fig4)
      dev.off()
      cat(paste0("  Full contact pattern plot saved to: ", output_file, "\n"))
    } else {
      cat(paste0("  Warning: Could not generate full contact pattern plot for ", country, "\n"))
    }
  }, error = function(e) {
    cat(paste0("  ERROR creating full contact pattern plot for ", country, ": ", e$message, "\n"))
  })
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
tryCatch({
  clustering_result <- cluster_countries(c(comix_countries, non_comix_countries))
  cat("Clustering completed successfully.\n")
}, error = function(e) {
  cat(paste0("ERROR in clustering: ", e$message, "\n"))
  clustering_result <- NULL
})

# Match non-CoMix countries to similar CoMix countries
# This helps identify which CoMix country's data can best represent each non-CoMix country
cat("\nMatching non-CoMix countries to similar CoMix countries...\n")
tryCatch({
  match_results <- match_countries(comix_countries, non_comix_countries,
                                   top_n = 3,
                                   cluster_results = clustering_result)
  cat("Matching completed successfully.\n")
}, error = function(e) {
  cat(paste0("ERROR in matching: ", e$message, "\n"))
  match_results <- NULL
})

# Visualize the matching results to show which countries are most similar
if (!is.null(match_results)) {
  cat("\nVisualizing country matching results...\n")
  tryCatch({
    match_df <- visualize_matches(match_results, plot_type = "both", show_top_n = 2)
    cat("Matching visualization completed.\n")
  }, error = function(e) {
    cat(paste0("ERROR in visualization: ", e$message, "\n"))
  })
}

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
  
  tryCatch({
    generate_CM_predictions(country_1, country_2, n_age_groups, save_matrices = TRUE)
    cat(sprintf("  Predictions generated successfully for: %s -> %s\n", country_1, country_2))
  }, error = function(e) {
    cat(sprintf("  ERROR generating predictions for %s -> %s: %s\n", country_1, country_2, e$message))
  })
}

# Create visualizations of the predicted contact matrices
cat("\nCreating visualization plots for non-CoMix country predictions...\n")
for (pair in country_pairs) {
  country_1 <- pair[1]  # CoMix country (source)
  country_2 <- pair[2]  # Non-CoMix country (target)
  
  # Construct CSV path with age group suffix
  csv_path <- paste0("outputs_level2/CM/", country_2, "_from_", country_1, "_", n_age_groups, "groups_contact_matrices.csv")
  
  # If file with suffix doesn't exist, try without suffix
  if (!file.exists(csv_path)) {
    csv_path <- paste0("outputs_level2/CM/", country_2, "_from_", country_1, "_contact_matrices.csv")
  }
  
  cat(sprintf("Creating figure for: %s -> %s\n", country_1, country_2))
  
  tryCatch({
    fig5 <- plot_CM_from_csv(csv_path, n_age_groups, age_group_labels)
    
    if (!is.null(fig5)) {
      output_dir <- "figures/fit_C_NPI/noCoMix/"
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      
      output_file <- paste0(output_dir, country_2, "_from_", country_1, "_", n_age_groups, "groups_contacts.png")
      
      # Adjust plot size based on number of age groups
      plot_size <- max(3000, n_age_groups * 500)
      
      png(filename = output_file, width = plot_size, height = 1875)
      print(fig5)
      dev.off()
      cat(sprintf("  Figure saved to: %s\n", output_file))
    } else {
      cat(sprintf("  Warning: Could not create figure for %s -> %s\n", country_1, country_2))
    }
  }, error = function(e) {
    cat(sprintf("  ERROR creating figure for %s -> %s: %s\n", country_1, country_2, e$message))
  })
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("Level 2 analysis completed.\n")
cat("Configuration used:\n")
cat("- Number of age groups:", n_age_groups, "\n")
cat("- Age group labels:", paste(age_group_labels, collapse = ", "), "\n")
cat("- Countries processed:", length(countries), "\n")
cat("- Output files saved with suffix: '_", n_age_groups, "groups'\n")
cat("=============================================================================\n")