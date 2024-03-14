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

n_chains <- 4
n_warmups <- 1000
n_iter <- 4000
n_thin <- 1  #default
n_adapt_delta <- 0.95 #default 0.8
n_max_treedepth <- 10 #default
n_knots <- 50
s_degree <- 3

a <- c(0.3,1,5)
t <- c(0.5,5,10)
s <- c(0.5,5,10)

combinations <- expand.grid(a = a, t = t, s = s)
combo <- nrow(combinations)
# Specify parameters to monitor
parameters = c("beta1", "beta2", "beta3", "beta4", 
               "tau1", "tau2", "tau3", "tau4", 
               "sigma",
               "y_pred", "log_lik", "dev")


for (i in 1:combo) {
  n_aa <- combinations[i,1]
  n_tt <- combinations[i,2]
  n_ss <- combinations[i,3]
  fit <- nuts_fit("AT", n_chains, n_warmups, n_iter, n_thin, n_adapt_delta, n_max_treedepth,aa,tt,ss)
  #save.image(file = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/TVEM_results/knots10/',country, '_TVEM.RData'))
  save.image(file = paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/TVEM_results/knots50/test_AT',combo, '_TVEM.RData'))
}
