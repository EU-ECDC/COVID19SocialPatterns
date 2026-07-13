#' Extract CoMix Survey Date Ranges by Wave for a Specific Country
#'
#' This function loads CoMix survey date information from a CSV file for a given country
#' and calculates the start and end dates for each survey wave.
#'
#' @param country A character string specifying the country code.
#'                This will be used to construct the filename and determine wave subsetting.
#'
#' @return A data frame containing start and end dates for each survey wave.
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
  
  # Create a data frame with start and end dates for each wave
  # Start date is the minimum sday_id for each wave
  # End date is the day before the next wave's start date (or the maximum sday_id for the last wave)
  comix_date_ranges <- data.frame(
    start_date <- as.Date(gsub("\\.", "-", c(min_values))),
    end_date <- c(start_date[-1] - 1, 
                  as.Date(gsub("\\.", "-", max((comix_dates %>% filter(wave == max(comix_dates$wave)))$sday_id)))))
  
  # Apply country-specific subsetting
  if (country=="BE"){
    comix_date_ranges <- comix_date_ranges[9:43,]
  }else if (country=="NL"){
    comix_date_ranges <- comix_date_ranges[9:28,]
  }else {comix_date_ranges<- comix_date_ranges}
  
  return(comix_date_ranges)
}
