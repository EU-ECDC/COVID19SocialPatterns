# ==============================================================================
# This script runs the full pipeline in three main steps:
# 1. Creates EU map with data coverage and POLYMOD and CoMix heatmaps for each country
# 2. Runs Level 1 TVEM analysis (by calling main_level1.R)
# 3. Runs Level 2 MVR analysis (by calling main_level2.R)
# ==============================================================================

# Load required libraries for the coverage map generation
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(reshape2)
library(tidyr)

# Load contact_data_coverage.R functions
source("R/level_1/contact_data_coverage.R")
source("R/level_1/contact_data.R")
source("R/level_2/comix_waves_dates.R")

# Define countries for analysis
countries <- c("AT", "BE", "HR", "DK", "EE", "FI", "FR",
               "GR", "HU", "IT", "LT", "NL", "PL", "PT",
               "SK", "SI", "ES", "UK")

# Step 1: Create EU map and heatmaps for data visualization
cat("\n============================================\n")
cat("Creating EU map with data coverage visualization...\n")

output_file <- "figures/eu_eea_contact_data_map.png"
eu_map <- create_eu_eea_contact_map(
  save_plot = TRUE,
  output_path = output_file
)
cat(paste0("EU map created and saved to: ", output_file, "\n"))

# Create POLYMOD and CoMix heatmaps for each country
cat("\n============================================\n")
cat("Generating POLYMOD and CoMix heatmaps...\n")

# Define output directories
polymod_output_dir <- "figures/heatmaps/POLYMOD"
comix_output_dir <- "figures/heatmaps/CoMix"
dir.create(polymod_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(comix_output_dir, recursive = TRUE, showWarnings = FALSE)

for (country in countries) {
  cat(paste0("Generating heatmaps for country: ", country, "\n"))
  
  # Create temporary directories for the functions to use
  country_heatmap_dir <- paste0("heatmaps_", country)
  country_figures_dir <- file.path(country_heatmap_dir, "figures")
  
  # Create these directories if they don't exist
  if (!dir.exists(country_heatmap_dir)) {
    dir.create(country_heatmap_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(country_figures_dir)) {
    dir.create(country_figures_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Generate POLYMOD heatmaps
  tryCatch({
    cat(paste0("  Generating POLYMOD heatmaps for ", country, "...\n"))
    POLYMOD_result <- POLYMOD_heatmaps(country)
    
    # Copy files to the final destination and clean up
    polymod_files <- list.files(country_figures_dir, pattern = "POLYMOD_heatmap_.*\\.png", full.names = TRUE)
    if (length(polymod_files) > 0) {
      # Create country subfolder in the POLYMOD directory
      country_polymod_dir <- file.path(polymod_output_dir, country)
      dir.create(country_polymod_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Copy files to the country-specific folder
      file.copy(polymod_files, country_polymod_dir, overwrite = TRUE)
      cat(paste0("  POLYMOD heatmaps copied to: ", country_polymod_dir, "\n"))
      
      # Remove original files to avoid duplication
      file.remove(polymod_files)
    }
  }, error = function(e) {
    cat(paste0("  Error generating POLYMOD heatmaps for ", country, ": ", e$message, "\n"))
  })
  
  # Generate CoMix heatmaps
  tryCatch({
    cat(paste0("  Generating CoMix heatmaps for ", country, "...\n"))
    CoMix_result <- CoMix_heatmaps_real_time(country)
    
    # Copy files to the final destination and clean up
    comix_files <- list.files(country_figures_dir, pattern = "CoMix_heatmap_.*\\.png", full.names = TRUE)
    if (length(comix_files) > 0) {
      # Create country subfolder in the CoMix directory
      country_comix_dir <- file.path(comix_output_dir, country)
      dir.create(country_comix_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Copy files to the country-specific folder
      file.copy(comix_files, country_comix_dir, overwrite = TRUE)
      cat(paste0("  CoMix heatmaps copied to: ", country_comix_dir, "\n"))
      
      # Remove original files to avoid duplication
      file.remove(comix_files)
    }
  }, error = function(e) {
    cat(paste0("  Error generating CoMix heatmaps for ", country, ": ", e$message, "\n"))
  })
  
  # Clean up the temporary directories after processing each country
  unlink(country_heatmap_dir, recursive = TRUE)
}

cat("Heatmaps generation completed.\n")

# Step 2: Run Level 1 analysis
cat("\n============================================\n")
cat("Running Level 1: Time-Varying Effect Models...\n")

# Run the main_level1.R script
source("R/level_1/main_level1.R")
cat("Level 1 analysis completed.\n")

# Step 3: Run Level 2 analysis
cat("\n============================================\n")
cat("Running Level 2: Multivariate Regression Models...\n")

# Run the main_level2.R script
source("R/level_2/main_level2.R")
cat("Level 2 analysis completed.\n")

cat("\n============================================\n")
cat("Analysis completed successfully!\n")
cat("============================================\n")



