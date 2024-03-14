contact_data <- function(country){
  polymod_path <- 'C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/Polymod/'
  comix_path <- 'C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/contact_matrix_EC/'
  
  polymod_file <- file.path(polymod_path, paste0(country, '.csv'))
  comix_file <- file.path(comix_path, paste0(country, '.csv'))
  
  if (!file.exists(polymod_file) | !file.exists(comix_file)){
    print("Files do not exist.")
    return(NULL)
  }
  
  polymod <- read.csv(polymod_file)
  
  if (country=="BE_ext"){
    comix <- read.csv(comix_file)
    comix <- comix[9:43,]
  }else {comix<- read.csv(comix_file)}
  
  return(list(polymod = polymod, comix = comix))
}


data_to_stan_list <- function(country,aa,tt,ss){
  stan_data_penalised <- list(
  I = ncol(contact_data(country)[[2]][,-1]),
  T = nrow(contact_data(country)[[2]][,-1]),
  K = nrow(contact_data(country)[[1]][,]),
  num_knots = n_knots,
  spline_degree = s_degree,
  knots = unname(quantile(c(1:nrow(contact_data(country)[[2]][,-1])),probs=seq(from=0, to=1, length.out = n_knots))),
  Y = as.matrix(contact_data(country)[[2]][,-1]),
  X = t(as.matrix(contact_data(country)[[1]][,])),
  time = c(1:nrow(contact_data(country)[[2]][,-1]),
  aa = n_aa,
  tt = n_tt,
  ss = n_ss)
  )
  
  return(stan_data_penalised)
}
