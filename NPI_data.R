NPI_data <- function(country){
  NPI_path <- 'C:/Users/zd22230/OneDrive - University of Bristol/ContactPatternsAC/Rfiles/NPI/'
  NPI_file <- file.path(NPI_path, paste0(country, '.csv'))
  
  if (!file.exists(NPI_file)){
    print("Files do not exist.")
    return(NULL)
  }
  
  NPI <- read.csv(NPI_file)
  
  return(NPI)
}