library(rstan)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggcorrplot)
rstan_options(auto_write = TRUE)           
options(mc.cores = parallel::detectCores())
set.seed(1234)

source("contact_data.R")
source("NPI_data.R")
source("comix_waves_dates.R")
source("regression_data.R")
source("fit_level1.R")
source("fit_level2.R")
source("figures_level1.R")
source("figures_level2.R")


countries <- c("AT", "BE", "HR", "DK", "EE", "FI", 
               "FR", "GR", "HU", "IT", "LT", "NL", 
               "PL", "PT", "SK", "SI", "ES", "CH")

n_chains=4
n_warmups=1000
n_iter=4000
n_thin=1
n_adapt_delta=0.95 #default 0.8
n_max_treedepth=10 #default

# Specify parameters to monitor
parameters = c("delta0","delta", "L_Omega", "L_sigma",
                          "mu", "L_Sigma", 
                          "log_lik", "dev", "Sigma", "Omega", "y_pred")

for (country in countries) {
  fit <- nuts_reg(country, n_chains, n_warmups, n_iter, n_thin, n_adapt_delta, n_max_treedepth)
  #save.image(file = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/MVNreg_results/knots10/',country, '_TVEM.RData'))
  save.image(file = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/MVNreg_results/knots50/',country, '_TVEM.RData'))
}

for (country in countries) {
  fig3 <- fitted_median_beta(country)
  
  if (!is.null(fig3)) {
    png(filename = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/Figures/fit_beta_knots50_NPI/', country, '_fitted_contacts.png'),
        width = 1875, height = 1875)  # Set the width and height as per your requirements
    print(fig3)
    dev.off()
  }
}




