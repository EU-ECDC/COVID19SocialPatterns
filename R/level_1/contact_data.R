contact_data <- function(country){
  polymod_path <- 'data/POLYMOD/'
  comix_path <- 'data/CoMix/'
  
  polymod_file <- file.path(polymod_path, paste0(country, '.csv'))
  comix_file <- file.path(comix_path, paste0(country, '.csv'))
  
  if (file.exists(polymod_file) & !file.exists(comix_file)){
    comix_file <- file.path(comix_path, paste0("AT", '.csv'))
  }
  
  if (!file.exists(polymod_file) & !file.exists(comix_file)){
    print("Files do not exist.")
    return(NULL)
  }
  
  polymod <- read.csv(polymod_file)
  
  if (country=="BE_ext"|country=="BE_four"){
    comix <- read.csv(comix_file)
    comix <- comix[9:43,]
  }else {comix<- read.csv(comix_file)}
  
  return(list(polymod = polymod, comix = comix))
  #return(list(polymod = polymod, comix = comix[1:30,])) #to test extrapolation
}


data_to_stan_list <- function(country){
  stan_data_penalised <- list(
  I = ncol(contact_data(country)[[2]][,-1]),
  T = nrow(contact_data(country)[[2]][,-1]),
  K = nrow(contact_data(country)[[1]][,]),
  num_knots = n_knots,
  spline_degree = s_degree,
  knots = unname(quantile(c(1:nrow(contact_data(country)[[2]][,-1])),probs=seq(from=0, to=1, length.out = n_knots))),
  Y = as.matrix(contact_data(country)[[2]][,-1]),
  X = t(as.matrix(contact_data(country)[[1]][,])),
  time = c(1:nrow(contact_data(country)[[2]][,-1])),
  s_aa = n_aa,
  s_tt1 = n_tt1,
  s_tt2 = n_tt2,
  s_tt3 = n_tt3,
  s_tt4 = n_tt4,
  s_ss = n_ss)
  
  return(stan_data_penalised)
}
