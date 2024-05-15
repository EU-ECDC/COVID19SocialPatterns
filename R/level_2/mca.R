mca <- function(country){
  
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
  NPI_PD_comix <- NPI_PD
  NPI_PD_comix <- NPI_PD_comix[, -which(names(NPI_PD_comix) %in% c("date"))]
  NPI_PD_var <- NPI_PD_comix%>%
    dplyr::select(where(~ length(unique(.)) > 1))
  
  # apply MCA
  mca1 <-  MCA(NPI_PD_var, graph = FALSE)
  
  return(mca1)
}

percentage_var<- function(country){
  
  mca_result <- mca(country)
  
  cumulative_variance <- mca_result$eig$variance.cumulative

  # Calculate the percentage of variability explained by the first 5 dimensions
  percentage_explained <- cumulative_variance[5] * 100

return(percentage_explained)
}

MCA_plot <- function(country){

  scree_plot <- fviz_screeplot(mca(country), addlabels = TRUE, ylim = c(0, 90))
  
  # Contributions of rows to dimension 1
  dim1 <- fviz_contrib(mca(country), choice = "var", axes = 1, top = 15)
  # Contributions of rows to dimension 2
  dim2 <- fviz_contrib(mca(country), choice = "var", axes = 2, top = 15)
  # Contributions of rows to dimension 3
  dim3 <- fviz_contrib(mca(country), choice = "var", axes = 3, top = 15)
  
  combined_plot <- scree_plot + dim1 + dim2 + dim3 + plot_layout(ncol = 2, nrow = 2)

  return(combined_plot)
  }


