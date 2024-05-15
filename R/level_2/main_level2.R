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

source("R/level_1/contact_data.R")
source("R/level_1/fit_level1.R")
source("R/level_2/NPI_data.R")
source("R/level_2/comix_waves_dates.R")
source("R/level_2/regression_data.R")
source("R/level_2/fit_level2.R")
source("R/level_2/figures_level2.R")
source("R/level_2/reg_dep_indep.R")
source("R/level_2/mca.R")
source("R/level_2/noComix_C.R")

#Countries with CoMix data
countries <- c("AT", "BE", "HR", "DK", "EE",
                 "FI", "FR", "GR", "HU",
                 "IT", "LT", "NL", "PL",
                 "PT", "SK", "SI", "ES")
percentiles <- c("50","2.5","97.5")

n_chains=4
n_warmups=5#1000
n_iter=10#4000
n_thin=1
n_adapt_delta=0.95 
n_max_treedepth=10

# Specify parameters to monitor
parameters = c("delta0","delta", "L_Omega", "L_sigma",
               "mu", "L_Sigma", 
               "log_lik", "dev", "Sigma", "Omega",
               "mu_pred", "beta_pred","C_pred")

for (country in countries) {
  for (percentile in percentiles){
  fig1 <- estBeta_mcaNPI(country, percentile)
  
  if (!is.null(fig1)) {
    png(filename = paste0('figures/reg_data/', country, '_', percentile,  '_dep_indep.png'),
        width = 900, height = 600)  # Set the width and height as per your requirements
    
    print(fig1)
    dev.off()
    }
  }
}

for (country in countries) {
  mca_fig <- MCA_plot(country)
  if (!is.null(mca_fig)) {
    png(filename = paste0('figures/mca_plots/', country, '_mca.png'),
        width = 1000, height = 1000)  # Set the width and height as per your requirements
    
    print(mca_fig)
    dev.off()
  }
}

for (country in countries) {
  for (percentile in percentiles){
    fit <- nuts_reg(country, percentile, n_chains, n_warmups, n_iter, n_thin, n_adapt_delta, n_max_treedepth)
    save.image(file = paste0('outputs_level2/',country, '_', percentile, '_MVR.RData'))
  }
}

for (country in countries) {
  for (percentile in percentiles){
    fig2 <- fitted_median_beta(country, percentile)
    if (!is.null(fig2)) {
      png(filename = paste0('figures/fit_beta_NPI/', country,'_', percentile, '_beta.png'),
          width = 2500, height = 1875)  # Set the width and height as per your requirements
      print(fig2)
      dev.off()
    }
  }
}

for (country in countries) {
  fig3 <- pred_CM(country,"50")
  
  if (!is.null(fig3)) {
    png(filename = paste0('figures/fit_C_NPI/', country, '_contacts.png'),
        width = 3000, height = 1875)  # Set the width and height as per your requirements
    print(fig3)
    dev.off()
  }
}

for (country in countries) {
  fig4 <- pred_CM_full(country)
  if (!is.null(fig4)) {
    png(filename = paste0('figures/fit_C_NPI/', country, '_contacts_full.png'),
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
    png(filename = paste0('figures/fit_C_NPI/noCoMix/', country_2, '_contacts.png'),
        width = 3000, height = 1875)  # Set the width and height as per your requirements
    print(fig5)
    dev.off()
  }
}



