library(rstan)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
rstan_options(auto_write = TRUE)           
options(mc.cores = parallel::detectCores())
set.seed(1234)

source("contact_data.R")
source("fit_level1.R")
source("figures_level1.R")

countries <- c("AT", "BE", "HR", "DK", "EE", "FI", "FR", "GR", "HU", 
                "IT", "LT", "NL", "PL", "PT", "SK", "SI", "ES", "CH")

n_chains=4
n_warmups=1000
n_iter=4000
n_thin=1
n_adapt_delta=0.95 
n_max_treedepth=10
n_knots = 10
#n_knots = 50
s_degree =3

#model1 = "m1_penalised_test.stan"
model2 = "m2_penalised.stan"

n_aa <- 1
n_tt1 <- 5
n_tt2 <- 5
n_tt3 <- 5
n_tt4 <- 5
n_ss <- 5

# # Specify parameters to monitor
# parameters = c("beta1", "beta2", "beta3", "beta4", 
#                "tau1", "tau2", "tau3", "tau4", 
#                "sigma",
#                "y_pred", "log_lik", "dev")

parameters = c("beta0", "beta1", "beta2", "beta3", "beta4", 
               "tau1", "tau2", "tau3", "tau4", 
               "sigma",
               "y_pred", "log_lik", "dev")


for (country in countries) {
  fit <- nuts_fit(model2,country, n_chains, n_warmups, n_iter, n_thin, n_adapt_delta, n_max_treedepth)
  save.image(file = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/TVEM_results/knots10_interc/',country, '_TVEM.RData'))
  #save.image(file = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/TVEM_results/knots50/',country, '_TVEM.RData'))
}

for (country in countries) {
  fig1 <- fitted_beta(country)
  
  if (!is.null(fig1)) {
    png(filename = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/Figures/fit_beta_knots10_interc/', country, '_fitted_beta.png'),
        width = 1000, height = 800)  # Set the width and height as per your requirements
    # png(filename = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/Figures/fit_beta_knots50/', country, '_fitted_contacts.png'),
    #     width = 1000, height = 800)  # Set the width and height as per your requirements
    print(fig1)
    dev.off()
  }
}

for (country in countries) {
  fig2 <- fitted_contacts(country)
  
  if (!is.null(fig2)) {
    png(filename = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/Figures/fitC_knots10_interc/', country, '_fitted_contacts.png'),
        width = 1875, height = 1875)  # Set the width and height as per your requirements
    print(fig2)
    dev.off()
  }
}
