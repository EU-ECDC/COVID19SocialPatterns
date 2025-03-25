#' Set up POLYMOD contact matrices for all countries
#' 
#' This function creates country-specific contact matrices by loading raw contact data,
#' aggregating it into specific age groups, and saving the results as CSV files.
#' Each country's data is processed and saved using its ISO 2-letter code.
  polymod_setup<- function(){
  
  # Check if required data files exist
  required_files <- c(
    "data/contact_matrices_home.rda",
    "data/contact_matrices_work.rda",
    "data/contact_matrices_school.rda", 
    "data/contact_matrices_other_locations.rda"
  )
  
  for (file in required_files) {
    if (!file.exists(file)) {
      stop(paste("Required file not found:", file))
    }
  }
  
  # Load all necessary contact matrices from data files
  load("data/contact_matrices_home.rda")
  load("data/contact_matrices_work.rda")
  load("data/contact_matrices_school.rda")
  load("data/contact_matrices_other_locations.rda")
  
  # Define mapping of country names to their ISO 2-letter codes
  # This will be used for file naming
  countries <- c(
    "Austria" = "AT",
    "Belgium" = "BE",
    "Croatia" = "HR",
    "Denmark" = "DK",
    "Estonia" = "EE",
    "Finland" = "FI",
    "France" = "FR",
    "Greece" = "GR",
    "Hungary" = "HU",
    "Italy" = "IT",
    "Lithuania" = "LT",
    "Netherlands" = "NL",
    "Poland" = "PL",
    "Portugal" = "PT",
    "Slovakia" = "SK",
    "Slovenia" = "SI",
    "Spain" = "ES",
    "United Kingdom" = "UK",
    "Bulgaria" = "BG",
    "Cyprus" = "CY",
    "Czechia" = "CZ",
    "Germany" = "DE",
    "Iceland" = "IS",
    "Ireland" = "IE",
    "Latvia" = "LV",
    "Luxembourg" = "LU",
    "Malta" = "MT",
    "Romania" = "RO",
    "Sweden" = "SE"
  )
  
  # Directory to save the files
  folder_path <- "data/POLYMOD"
  
  # Create the directory if it doesn't exist
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  
  # Process each country's data
  for (country in names(countries)) {
    country_code <- countries[country]
    # Handle special cases with different naming conventions in datasets
    dataset_country <- if (country == "United Kingdom") "United Kingdom of Great Britain" 
    else if (country == "Czechia") "Czech Republic"
    else country
    # Extract contact matrices for each setting (home, work, school, other)
    conmat_home <- data.frame(contact_matrices_home[[dataset_country]])
    conmat_work <- data.frame(contact_matrices_work[[dataset_country]])
    conmat_school <- data.frame(contact_matrices_school[[dataset_country]])
    conmat_other_locations <- data.frame(contact_matrices_other_locations[[dataset_country]])
    
    # Define the standard age groups used in the original data
    age_groups <- c("0-4",
                    "5-9",
                    "10-14",
                    "15-19",
                    "20-24",
                    "25-29",
                    "30-34",
                    "35-39",
                    "40-44",
                    "45-49",
                    "50-54",
                    "55-59",
                    "60-64",
                    "65-69",
                    "70-74",
                    "75+")
    
    # Get age distribution for the country using Bernadette package
    age_distr <- Bernadette::age_distribution(country = country, year = 2020)
    # Create a mapping table to aggregate from 16 age groups to 4 broader groups
    # Maps original age groups to: 0-18, 19-44, 45-64, 65+
    lookup_table <- data.frame(Initial = age_distr$AgeGrp,
                               Mapping = c(rep("0-18",  4),
                                           rep("19-44", 5),
                                           rep("45-64", 4),
                                           rep("65+"  , 3)))
    
    # Set row and column names
    rownames(conmat_home) <- colnames(conmat_home) <- age_groups
    rownames(conmat_work) <- colnames(conmat_work) <- age_groups
    rownames(conmat_school) <- colnames(conmat_school) <- age_groups
    rownames(conmat_other_locations) <- colnames(conmat_other_locations) <- age_groups
    
    aggr_age <- Bernadette::aggregate_age_distribution(age_distr, lookup_table)
    # Aggregate the contact matrices
    aggr_cm_home <- Bernadette::aggregate_contact_matrix(conmat_home, lookup_table, aggr_age)
    aggr_cm_work <- Bernadette::aggregate_contact_matrix(conmat_work, lookup_table, aggr_age)
    aggr_cm_school <- Bernadette::aggregate_contact_matrix(conmat_school, lookup_table, aggr_age)
    aggr_cm_other_locations <- Bernadette::aggregate_contact_matrix(conmat_other_locations, lookup_table, aggr_age)
    
    # Rearrange to 4 age groups
    aggr_cm_home <- aggr_cm_home[, c("0-18", "19-44", "45-64", "65+")]
    aggr_cm_home <- aggr_cm_home[c("0-18", "19-44", "45-64", "65+"), ]
    aggr_cm_work <- aggr_cm_work[, c("0-18", "19-44", "45-64", "65+")]
    aggr_cm_work <- aggr_cm_work[c("0-18", "19-44", "45-64", "65+"), ]
    aggr_cm_school <- aggr_cm_school[, c("0-18", "19-44", "45-64", "65+")]
    aggr_cm_school <- aggr_cm_school[c("0-18", "19-44", "45-64", "65+"), ]
    aggr_cm_other_locations <- aggr_cm_other_locations[, c("0-18", "19-44", "45-64", "65+")]
    aggr_cm_other_locations <- aggr_cm_other_locations[c("0-18", "19-44", "45-64", "65+"), ]
    
    # Combine the matrices
    polymod <- rbind(unlist(aggr_cm_home),
                     unlist(aggr_cm_work),
                     unlist(aggr_cm_school),
                     unlist(aggr_cm_other_locations))
    
    # Generate the file path
    file_path <- file.path(folder_path, paste0(country_code, ".csv"))
    
    # Write the data to a CSV file
    write.table(polymod, file = file_path, sep = ",", row.names = FALSE, col.names = TRUE)
    
    # Print a message
    message(paste("Saved", country, "data to", file_path))
  }
}

#' Load contact data for a specified country
#' 
#' This function loads POLYMOD and CoMix contact data for a specified country.
#' If the POLYMOD data doesn't exist, it runs the polymod_setup function to generate it.
#' If the CoMix data doesn't exist for the specified country, it uses Austria as a fallback.
#' 
#' @param country A character string representing the ISO 2-letter country code
#' @return A list containing two data frames: polymod and comix with contact matrices
  contact_data <- function(country){
  polymod_path <- 'data/POLYMOD/'
  comix_path <- 'data/CoMix/'
  
  polymod_file <- file.path(polymod_path, paste0(country, '.csv'))
  comix_file <- file.path(comix_path, paste0(country, '.csv'))
  
  # Check if POLYMOD directory exists, if not create it
  if (!dir.exists(polymod_path)) {
    dir.create(polymod_path, recursive = TRUE)
  }
  
  # If POLYMOD file doesn't exist, generate it
  if (!file.exists(polymod_file)) {
    # Run the polymod_setup function to generate the needed files
    polymod_setup()
  }
  
  # If POLYMOD file still doesn't exist after setup, there's an issue
  if (!file.exists(polymod_file)) {
    warning(paste("Could not generate POLYMOD data for country code:", country))
    return(NULL)
  }
  
  # If CoMix file doesn't exist, use Austria as fallback
  if (!file.exists(comix_file)) {
    comix_file <- file.path(comix_path, "AT.csv")
    if (!file.exists(comix_file)) {
      warning("Neither requested country nor fallback Austria CoMix data exist.")
      return(NULL)
    }
  }
  
  polymod <- read.csv(polymod_file)
 
  # Handle special cases for Belgium and Netherlands 
  if (country=="BE"){
    comix <- read.csv(comix_file)
    comix <- comix[9:43,]
  }else if (country=="NL"){
    comix <- read.csv(comix_file)
    comix <- comix[9:28,]
  }else {comix<- read.csv(comix_file)}
  
  return(list(polymod = polymod, comix = comix))
  #return(list(polymod = polymod, comix = comix[1:30,])) #to test extrapolation
}


#' Convert contact data to a Stan model input list
#' 
#' This function takes a country code, loads the corresponding contact data,
#' and formats it into a list suitable for use with a Stan model.
#' 
#' @param country A character string representing the ISO 2-letter country code
#' @return A list containing all necessary data for the Stan model

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
