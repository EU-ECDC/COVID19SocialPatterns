#' Fit a Multivariate Regression Model using Stan for a Given Country
#'
#' This function fits a multivariate regression model with out-of-sample predictions
#' using Stan. It processes data for a specified country and percentile threshold,
#' then runs the Stan model with the provided MCMC parameters.
#'
#' @param country A character string specifying the country code.
#' @param percentile A numeric value indicating the percentile threshold for data processing.
#' @param n_chains An integer specifying the number of MCMC chains to run.
#' @param n_warmups An integer specifying the number of warmup iterations.
#' @param n_iter An integer specifying the total number of iterations (including warmup).
#' @param n_thin An integer specifying the thinning interval for MCMC samples.
#' @param n_adapt_delta A numeric value between 0 and 1 controlling the target acceptance rate.
#' @param n_max_treedepth An integer specifying the maximum tree depth for the NUTS sampler.
#'
#' @return A fitted Stan model object containing the posterior samples.
nuts_reg <- function(country, percentile, n_chains, n_warmups, n_iter, n_thin, 
                     n_adapt_delta,n_max_treedepth){
  
  reg_by_country <-  stan(file.path("stan_models","MVR_out_of_sample.stan"), 
                          data = reg_data_to_stan_list(country, percentile), 
                          pars = parameters , 
                          #init = inits(country), 
                          chains = n_chains, 
                          warmup = n_warmups, 
                          iter = n_iter, 
                          thin=n_thin, 
                          control=list(adapt_delta=n_adapt_delta, max_treedepth=n_max_treedepth), seed=4321)
  
  return(reg_by_country)
}

#' Load a Fitted Regression Model from Saved RData File
#'
#' This function loads a previously fitted multivariate regression model for a given
#' country and percentile threshold from an RData file. It checks if the country is
#' valid and if the file exists before attempting to load the data.
#'
#' @param country A character string specifying the country code.
#' @param percentile A numeric value indicating the percentile threshold used in the model.
#'
#' @return A fitted Stan model object containing the posterior samples.
load_reg_fit <- function(country, percentile) {
  valid_countries <- c("AT", "BE", "HR", "DK", "EE", 
                       "FI", "FR", "GR", "HU", 
                       "IT", "LT", "NL", "PL", 
                       "PT", "SK", "SI", "ES")
  
  if (!country %in% valid_countries) {
    stop("Invalid country. Please choose from: ", paste(valid_countries, collapse = ", "))
  }
  
  data_file <- paste0('outputs_level2/', country, '_', percentile, '_MVR.RData')
  
  if (file.exists(data_file)) {
    loaded_env <- new.env()
    load(data_file, envir = loaded_env)
    
    loaded_data <- loaded_env$fit
    return(loaded_data)
  } else {
    stop("RData file not found for the given country.")
  }
}