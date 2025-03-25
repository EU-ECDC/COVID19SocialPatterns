#' Fit a Stan model for a given country using NUTS sampler
#' 
#' This function runs a Stan model on contact data for a specified country
#' using the No-U-Turn Sampler (NUTS) algorithm for MCMC sampling.
#' 
#' @param model A compiled Stan model object
#' @param country A character string with the ISO 2-letter country code
#' @param n_chains Number of Markov chains to run
#' @param n_warmups Number of warmup iterations per chain
#' @param n_iter Total number of iterations per chain (including warmup)
#' @param n_thin Thinning rate (only keeping every n_thin samples)
#' @param n_adapt_delta Target acceptance rate
#' @param n_max_treedepth Maximum depth of the trees evaluated by NUTS
#' @return A fitted Stan model object
nuts_fit <- function(model, country, n_chains, n_warmups, n_iter, n_thin, 
                     n_adapt_delta,n_max_treedepth){
  fit_by_country <-  stan(model,
                          data = data_to_stan_list(country), 
                          pars = parameters , 
                          #init = inits(country), 
                          chains = n_chains, 
                          warmup = n_warmups, 
                          iter = n_iter, 
                          thin=n_thin, 
                          control=list(adapt_delta=n_adapt_delta, max_treedepth=n_max_treedepth), seed=4321)
  
  return(fit_by_country)
}

#' Load a previously fitted Stan model for a specified country
#' 
#' This function loads a pre-fitted Stan model from an RData file
#' based on the country code provided. It ensures the country is valid
#' and the corresponding file exists before attempting to load it.
#' 
#' @param country A character string with the ISO 2-letter country code
#' @return A fitted Stan model object loaded from the RData file
#' @throws Error if the country is invalid or the file doesn't exist
load_fit <- function(country) {
  valid_countries <- c("AT","BE","HR","DK","EE","FI","FR","GR","HU",
                       "IT","LT","NL","PL","PT","SK","SI","ES","UK")
  
  if (!country %in% valid_countries) {
    stop("Invalid country. Please choose from: ", paste(valid_countries, collapse = ", "))
  }
  data_file <- paste0('outputs_level1/',country, '_TVEM.RData')
  
  if (file.exists(data_file)) {
    loaded_env <- new.env()
    load(data_file, envir = loaded_env)
    
    loaded_data <- loaded_env$fit
    return(loaded_data)
  } else {
    stop("RData file not found for the given country.")
  }
}
