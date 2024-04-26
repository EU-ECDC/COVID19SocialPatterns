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
rstan_options(auto_write = TRUE)           
options(mc.cores = parallel::detectCores())
set.seed(1234)

source("contact_data.R")
source("NPI_data.R")
source("comix_waves_dates.R")
source("regression_data.R")
source("fit_level1.R")
source("fit_level2.R")
source("figures_level2.R")
source("reg_dep_indep.R")
source("mca.R")
source("noComix_C.R")

#Countries with CoMix data
countries <- c("AT", "BE", "HR", "DK", "EE",
                 "FI", "FR", "GR", "HU",
                 "IT", "LT", "NL", "PL",
                 "PT", "SK", "SI", "ES")
percentiles <- c("50","2.5","97.5")

n_chains=4
n_warmups=1000
n_iter=4000
n_thin=1
n_adapt_delta=0.95 
n_max_treedepth=10

# Specify parameters to monitor
parameters = c("delta0","delta", "L_Omega", "L_sigma",
               "mu", "L_Sigma", 
               "log_lik", "dev", "Sigma", "Omega",
               "mu_pred", "beta_pred","C_pred")

# Define the output directory 
output_dir <- Sys.getenv("OUTPUT_DIR")
if (is.null(output_dir)) {
  stop("Please set the environment variable OUTPUT_DIR to specify the output directory.")
}

# Temporarily set the OUTPUT_DIR variable within the R session before generating plots and stan outputs
Sys.setenv(OUTPUT_DIR = "C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/")
dir.create(paste0(output_dir, "/MVNreg_results/knots10_interc/dep_indep"))
dir.create(paste0(output_dir, "/MVNreg_results/knots10_interc/mca_plots"))
dir.create(paste0(output_dir, "/MVNreg_results/knots10_interc/fit_beta"))
dir.create(paste0(output_dir, "/MVNreg_results/knots10_interc/fit_C"))

for (country in countries) {
  for (percentile in percentiles){
  fig1 <- estBeta_mcaNPI(country, percentile)
  
  if (!is.null(fig1)) {
    png(filename = paste0(output_dir, "/MVNreg_results/knots10_interc/dep_indep/", country, '_', percentile,  '_dep_indep.png'),
        width = 900, height = 600)  # Set the width and height as per your requirements
    
    print(fig1)
    dev.off()
    }
  }
}

for (country in countries) {
  mca_fig <- MCA_plot(country)
  if (!is.null(mca_fig)) {
    png(filename = paste0(output_dir, "/MVNreg_results/knots10_pred/mca_plots/", country, '_mca_fig.png'),
        width = 1000, height = 1000)  # Set the width and height as per your requirements
    
    print(mca_fig)
    dev.off()
  }
}

for (country in countries) {
  for (percentile in percentiles){
    fit <- nuts_reg(country, percentile, n_chains, n_warmups, n_iter, n_thin, n_adapt_delta, n_max_treedepth)
    save.image(file = paste0(output_dir, "/MVNreg_results/knots10_interc/",country, '_', percentile, '_MVR.RData'))
  }
}

for (country in countries) {
  for (percentile in percentiles){
    fig2 <- fitted_median_beta(country, percentile)
    if (!is.null(fig2)) {
      png(filename = paste0(output_dir, "/MVNreg_results/knots10_interc/fit_beta/", country,'_', percentile, '_fit_betaNPI.png'),
          width = 2500, height = 1875)  # Set the width and height as per your requirements
      print(fig2)
      dev.off()
    }
  }
}

for (country in countries) {
  fig3 <- pred_CM(country)
  
  if (!is.null(fig3)) {
    png(filename = paste0(output_dir, "/MVNreg_results/knots10_interc/fit_C/", country, '_fit_C.png'),
        width = 3000, height = 1875)  # Set the width and height as per your requirements
    print(fig3)
    dev.off()
  }
}

for (country in countries) {
  fig4 <- pred_CM_full(country)
  if (!is.null(fig4)) {
    png(filename = paste0(output_dir, "/MVNreg_results/knots10_interc/fit_C/", country, '_fit_C_full.png'),
        width = 3000, height = 1875)  # Set the width and height as per your requirements
    print(fig4)
    dev.off()
  }
}
############################################################################################
# Define vectors of country pairs (CoMix - no CoMix)
country_pairs <- list(
  c("IT", "CZ"),
  c("SI", "BG"),
  c("IT", "CY"),
  c("FR", "IE"),
  c("ES", "LV"),
  c("BE", "LU"),
  c("HR", "RO"),
  c("FR", "SE"),
  c("BE", "MT"),
  c("SI", "IS"),
  c("FR", "DE")
  # Add more pairs as needed
)

for (pair in country_pairs) {
  country_1 <- pair[1]
  country_2 <- pair[2]

  fig5 <- pred_CM_noCoMix(country_1, country_2)
  if (!is.null(fig5)) {
    png(filename = paste0(output_dir, "/MVNreg_results/knots10_interc/fit_C/", country_2, '_fit_C.png'),
        width = 3000, height = 1875)  # Set the width and height as per your requirements
    print(fig5)
    dev.off()
  }
}



