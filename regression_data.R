#Dependent

fitted_beta_coefficients <- function(country){

loaded_fit <- load_fit(country)
posts <-  rstan::extract(loaded_fit)
median_beta1 = apply(posts$beta1, 2, median)
median_beta2 = apply(posts$beta2, 2, median)
median_beta3 = apply(posts$beta3, 2, median)
median_beta4 = apply(posts$beta4, 2, median)
dependent <- cbind(median_beta1,median_beta2,median_beta3,median_beta4)

comix_date_ranges <- comix_dates(country)
start_date <- min(comix_date_ranges[,1])
end_date <- max(comix_date_ranges[,2])
dates <- seq(start_date, end_date, by = "days")

# Create a data frame with dates
dependent_ext <- data.frame(Date = dates)
dependent_ext$beta1 <- NA
dependent_ext$beta2 <- NA
dependent_ext$beta3 <- NA
dependent_ext$beta4 <- NA

# Linearly interpolate betas based on comix date ranges
for (i in 1:(nrow(comix_date_ranges) - 1)) {
  date_range_start <- comix_date_ranges[i, ]
  date_range_end <- comix_date_ranges[i + 1, ]
  
  indices <- dependent_ext$Date >= date_range_start$start_date & dependent_ext$Date <= date_range_end$end_date
  
  # Linear interpolation for each beta
  alpha <- as.numeric(dependent_ext$Date[indices] - date_range_start$start_date) / as.numeric(date_range_end$end_date - date_range_start$start_date)
  dependent_ext$beta1[indices] <- (1 - alpha) * median_beta1[i] + alpha * median_beta1[i + 1]
  dependent_ext$beta2[indices] <- (1 - alpha) * median_beta2[i] + alpha * median_beta2[i + 1]
  dependent_ext$beta3[indices] <- (1 - alpha) * median_beta3[i] + alpha * median_beta3[i + 1]
  dependent_ext$beta4[indices] <- (1 - alpha) * median_beta4[i] + alpha * median_beta4[i + 1]
  
  # # Assign constant beta values for each date range
  # dependent_ext$beta1[indices] <- median_beta1[i]
  # dependent_ext$beta2[indices] <- median_beta2[i]
  # dependent_ext$beta3[indices] <- median_beta3[i]
  # dependent_ext$beta4[indices] <- median_beta4[i]
}


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

comix_date_ranges <- comix_dates(country)
NPI_PD_comix <- NPI_PD %>%
  filter(date>=min(comix_date_ranges[,1]))%>% 
  filter(date<=max(comix_date_ranges[,2]))

NPI_PD_comix <- NPI_PD_comix[, -which(names(NPI_PD_comix) %in% c("date"))]
NPI_PD_var <- NPI_PD_comix%>%
  select(where(~ length(unique(.)) > 1))

# apply MCA
mca1 = MCA(NPI_PD_var, graph = FALSE)

if (ncol(NPI_PD_var)>=5){
  independent <- as.data.frame(mca1$ind$coord[, 1:5])
}else if (ncol(NPI_PD_var)<5){
  independent <- as.data.frame(mca1$ind$coord[, 1:4])
}

return(independent)
}


reg_data_to_stan_list <- function(country){

  reg_data = list(n_obs = nrow(fitted_beta_coefficients(country)),
                  K = ncol(fitted_beta_coefficients(country))-1,  #number of regressions
                  J = ncol(mca_factors(country)), #number of covariates
                  x = mca_factors(country),
                  y = fitted_beta_coefficients(country)[,-1])
  
  return(reg_data)
}