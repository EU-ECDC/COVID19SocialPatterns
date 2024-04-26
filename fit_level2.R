nuts_reg <- function(country, percentile, n_chains, n_warmups, n_iter, n_thin, 
                     n_adapt_delta,n_max_treedepth){
  
  reg_by_country <-  stan("MVR_out_of_sample.stan", 
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

load_reg_fit <- function(country, percentile) {
  valid_countries <- c("AT", "BE", "HR", "DK", "EE", 
                       "FI", "FR", "GR", "HU", 
                       "IT", "LT", "NL", "PL", 
                       "PT", "SK", "SI", "ES")
  
  if (!country %in% valid_countries) {
    stop("Invalid country. Please choose from: ", paste(valid_countries, collapse = ", "))
  }
  
  data_file <- paste0('C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/MVNreg_results/knots10_interc/', country, '_', percentile, '_MVR.RData')
  
  if (file.exists(data_file)) {
    loaded_env <- new.env()
    load(data_file, envir = loaded_env)
    
    loaded_data <- loaded_env$fit
    return(loaded_data)
  } else {
    stop("RData file not found for the given country.")
  }
}