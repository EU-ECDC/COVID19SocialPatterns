#' Load Non-Pharmaceutical Interventions (NPI) Data for a Specific Country
#'
#' This function loads NPI data from a CSV file for a given country.
#' The CSV files are expected to be in the 'data/NPI/' directory
#' with filenames corresponding to country names (e.g., 'France.csv').
#'
#' @param country A character string specifying the country name.
#'                This will be used to construct the filename.
#'
#' @return A data frame containing the NPI data for the specified country.
NPI_data <- function(country){
  NPI_path <- 'data/NPI/'
  NPI_file <- file.path(NPI_path, paste0(country, '.csv'))
  
  if (!file.exists(NPI_file)){
    print("Files do not exist.")
    return(NULL)
  }
  
  NPI <- read.csv(NPI_file)
  
  return(NPI)
}