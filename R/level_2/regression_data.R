#Dependent

fitted_beta_coefficients <- function(country,percentile){

loaded_fit <- load_fit(country)
posts <-  rstan::extract(loaded_fit)
median_beta1 = c(apply(posts$beta1, 2, median),NA)
median_beta2 = c(apply(posts$beta2, 2, median),NA)
median_beta3 = c(apply(posts$beta3, 2, median),NA)
median_beta4 = c(apply(posts$beta4, 2, median),NA)

low_beta1 = c(apply(posts$beta1, 2, quantile, probs = c(0.025)),NA)
low_beta2 = c(apply(posts$beta2, 2, quantile, probs = c(0.025)),NA)
low_beta3 = c(apply(posts$beta3, 2, quantile, probs = c(0.025)),NA)
low_beta4 = c(apply(posts$beta4, 2, quantile, probs = c(0.025)),NA)

high_beta1 = c(apply(posts$beta1, 2, quantile, probs = c(0.975)),NA)
high_beta2 = c(apply(posts$beta2, 2, quantile, probs = c(0.975)),NA)
high_beta3 = c(apply(posts$beta3, 2, quantile, probs = c(0.975)),NA)
high_beta4 = c(apply(posts$beta4, 2, quantile, probs = c(0.975)),NA)

if (percentile=="50"){
  dependent <- cbind(median_beta1,median_beta2,median_beta3,median_beta4)
}else if (percentile=="2.5"){
  dependent <- cbind(low_beta1,low_beta2,low_beta3,low_beta4)
}else if (percentile=="97.5"){
dependent <- cbind(high_beta1,high_beta2,high_beta3,high_beta4)
}

times <- c(comix_dates(country)[,1],max(comix_dates(country)[,2]))
ext_dates <- seq(min(comix_dates(country)[,1]), max(comix_dates(country)[,2]), by = "days")
numeric_dates <- as.numeric(times - min(times)) + 1
wave <- c(1:length(times))

df1 <- tibble(x=numeric_dates,y=dependent[,1],wave)
df2 <- tibble(x=numeric_dates,y=dependent[,2],wave)
df3 <- tibble(x=numeric_dates,y=dependent[,3],wave)
df4 <- tibble(x=numeric_dates,y=dependent[,4],wave)

df1 %>% 
  mutate(gradient = (lead(y)-y)/(lead(x)-x)) %>% 
  complete(x = x:max(x)) %>% 
  fill(wave) %>% 
  group_by(wave) %>% 
  fill(gradient)%>% 
  mutate(y2=first(y) + (x-first(x)) *gradient)%>%
  mutate(y2=ifelse(!is.na(y2),y2,y))->df1_ext

df2 %>% 
  mutate(gradient = (lead(y)-y)/(lead(x)-x)) %>% 
  complete(x = x:max(x)) %>% 
  fill(wave) %>% 
  group_by(wave) %>% 
  fill(gradient)%>% 
  mutate(y2=first(y) + (x-first(x)) *gradient)%>%
  mutate(y2=ifelse(!is.na(y2),y2,y))->df2_ext

df3 %>% 
  mutate(gradient = (lead(y)-y)/(lead(x)-x)) %>% 
  complete(x = x:max(x)) %>% 
  fill(wave) %>% 
  group_by(wave) %>% 
  fill(gradient)%>% 
  mutate(y2=first(y) + (x-first(x)) *gradient)%>%
  mutate(y2=ifelse(!is.na(y2),y2,y))->df3_ext

df4 %>% 
  mutate(gradient = (lead(y)-y)/(lead(x)-x)) %>% 
  complete(x = x:max(x)) %>% 
  fill(wave) %>% 
  group_by(wave) %>% 
  fill(gradient)%>% 
  mutate(y2=first(y) + (x-first(x)) *gradient)%>%
  mutate(y2=ifelse(!is.na(y2),y2,y))->df4_ext

dependent_ext <- tibble(ext_dates,
                        df1_ext$y, df1_ext$y2,
                        df2_ext$y,df2_ext$y2,
                        df3_ext$y,df3_ext$y2,
                        df4_ext$y,df4_ext$y2)%>% 
  filter(!is.na(df1_ext$y2))%>%
  rename(Date = ext_dates,
         beta1_w = `df1_ext$y`,
         beta1 = `df1_ext$y2`,
         beta2_w = `df2_ext$y`,
         beta2 = `df2_ext$y2`,
         beta3_w = `df3_ext$y`,
         beta3 = `df3_ext$y2`,
         beta4_w = `df4_ext$y`,
         beta4 = `df4_ext$y2`,)

# dependent_ext %>% ggplot()+
#   geom_point(aes(x=ext_dates,y=`df1_ext$y`),colour='red',size=4)+
#   geom_point(aes(x=ext_dates,y=`df1_ext$y2`))
# dependent_ext %>% ggplot()+
#   geom_point(aes(x=ext_dates,y=`df2_ext$y`),colour='red',size=4)+
#   geom_point(aes(x=ext_dates,y=`df2_ext$y2`))
# dependent_ext %>% ggplot()+
#   geom_point(aes(x=ext_dates,y=`df3_ext$y`),colour='red',size=4)+
#   geom_point(aes(x=ext_dates,y=`df3_ext$y2`))
# dependent_ext %>% ggplot()+
#   geom_point(aes(x=ext_dates,y=`df4_ext$y`),colour='red',size=4)+
#   geom_point(aes(x=ext_dates,y=`df4_ext$y2`))

return(dependent_ext)
}



#Independent

mca_factors <- function(country){
  
NPI <- NPI_data(country)
NPI$date <- as.Date(NPI$date)

pattern <- "Physical.distancing"
matching_indices <- grepl(pattern, names(NPI))
df_var_name <- NPI[, matching_indices]
date_var_indices <- sapply(NPI, function(x) class(x) == "Date")
NPI_PD <- NPI[, c(which(date_var_indices & !matching_indices)[1], which(matching_indices))]
pattern_to_remove <- "Physical.distancing."
NPI_PD <- NPI_PD %>%
  rename_at(vars(-matches("date")), ~gsub(pattern_to_remove, "", .))

NPI_PD <- NPI_PD %>%
  rename_with(~ gsub("Closure.of.public.spaces", "CPS", .), 
              starts_with("Closure.of.public.spaces"))%>%
  rename_with(~ gsub("Private.gathering.restriction", "PrG", .), 
              starts_with("Private.gathering.restriction"))%>%
  rename_with(~ gsub("Public.gathering.restriction", "PbG", .), 
              starts_with("Public.gathering.restriction"))%>%
  rename_with(~ gsub("Closure.of.educational.institutions", "CEI", .), 
              starts_with("Closure.of.educational.institutions"))%>%
  rename_with(~ gsub("Stay.at.home", "SH", .), 
              starts_with("Stay.at.home"))%>%
  rename_with(~ gsub("Measures.for.special.populations", "SpP", .), 
              starts_with("Measures.for.special.populations"))%>%
  rename_with(~ gsub("Workplace.measures", "W", .), 
              starts_with("Workplace.measures"))

NPI_PD <- NPI_PD %>%
  mutate(across(-date, 
                ~ case_when(. == 0 ~ 1, 
                            . == 1 ~ 2, 
                            . == 2 ~ 3, 
                            TRUE ~ .))) %>%
  mutate(across(where(is.numeric), as.character))

# NPI_PD_comix <- NPI_PD %>%
#   filter(date>=min(comix_dates(country)[,1]))%>% 
#   filter(date<=max(comix_dates(country)[,1]))

# NPI_PD_comix <- NPI_PD_comix[, -which(names(NPI_PD_comix) %in% c("date"))]
# NPI_PD_var <- NPI_PD_comix%>%
#   select(where(~ length(unique(.)) > 1))

NPI_PD <- NPI_PD[, -which(names(NPI_PD) %in% c("date"))]
NPI_PD_var <- NPI_PD%>%
  dplyr::select(where(~ length(unique(.)) > 1))

# apply MCA
mca1 = MCA(NPI_PD_var, graph = FALSE)

if (ncol(NPI_PD_var)>=5){
  independent <- as.data.frame(mca1$ind$coord[, 1:5])
}else if (ncol(NPI_PD_var)<5){
  independent <- as.data.frame(mca1$ind$coord[, 1:4])
}

independent <- cbind(NPI$date,independent)

return(independent)
}


fitted_interc <- function(country,percentile){
  
  loaded_fit <- load_fit(country)
  posts <-  rstan::extract(loaded_fit)

  if (percentile=="50"){
    fit_interc <- apply(posts$beta0, 2, median)
  }else if (percentile=="2.5"){
    fit_interc <- apply(posts$beta0, 2, quantile, probs = c(0.025))
  }else if (percentile=="97.5"){
    fit_interc <- apply(posts$beta0, 2, quantile, probs = c(0.975))
  }
  return(fit_interc)
}


# reg_data_to_stan_list <- function(country){
# 
#   reg_data = list(n_obs = nrow(fitted_beta_coefficients(country)),
#                   K = ncol(fitted_beta_coefficients(country)[,-c(1,2,4,6,8)]),  #number of regressions
#                   J = ncol(mca_factors(country)), #number of covariates
#                   x = mca_factors(country),
#                   y = fitted_beta_coefficients(country)[,-c(1,2,4,6,8)]
#                   )
#   
#   return(reg_data)
# }

reg_data_to_stan_list <- function(country, percentile){

  reg_data = list(n_train = nrow((mca_factors(country) %>%
                                    filter(`NPI$date`>=min(comix_dates(country)[,1]))%>%
                                    filter(`NPI$date`<=max(comix_dates(country)[,1])))[,-1]),
                  n_pred = nrow((mca_factors(country))[,-1]),
                  K = ncol(fitted_beta_coefficients(country,percentile)[,-c(1,2,4,6,8)]),  #number of regressions
                  J = ncol(mca_factors(country))-1,  #number of factor dimensions
                  I = ncol(contact_data(country)[[2]][,-1]),
                  x_train= (mca_factors(country) %>%
                            filter(`NPI$date`>=min(comix_dates(country)[,1]))%>%
                            filter(`NPI$date`<=max(comix_dates(country)[,1])))[,-1],
                  x_pred = (mca_factors(country))[,-1],
                  beta_train = fitted_beta_coefficients(country,percentile)[,-c(1,2,4,6,8)],
                  beta0_train = fitted_interc(country,percentile),
                  X = t(as.matrix(contact_data(country)[[1]][,])))

  return(reg_data)
}
