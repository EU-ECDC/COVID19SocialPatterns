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