comix_dates <- function(country){
  comix_dates_path <- 'data/comix_dates/'
  comix_dates_file <- file.path(comix_dates_path, paste0(country, '.csv'))
  
  if (!file.exists(comix_dates_file)){
    print("Files do not exist.")
    return(NULL)
  }
  
  comix_dates <- read.csv(comix_dates_file)
  
  min_values <- numeric()
  
  # Loop through each wave and find the minimum sday_id
  for (i in 1:max(comix_dates$wave)) {
    wave_data <- comix_dates %>%
      filter(wave == i)
    min_sday_id <- min(wave_data$sday_id)
    
    # Append the minimum to the vector
    min_values <- c(min_values, min_sday_id)
  }
  
  comix_date_ranges <- data.frame(
    start_date <- as.Date(gsub("\\.", "-", c(min_values))),
    end_date <- c(start_date[-1] - 1, 
                  as.Date(gsub("\\.", "-", max((comix_dates %>% filter(wave == max(comix_dates$wave)))$sday_id)))))
  
  return(comix_date_ranges)
}
