comix_dates <- function(country){ 
  
  if (country == "AT"|country == "PL"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2020-12-22", "2021-01-07", "2021-01-20", "2021-02-17",
                              "2021-03-03", "2021-03-17", "2021-04-14")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-04-19")))
  }
  
  
  else if (country == "BE"){
  comix_date_ranges <- data.frame(
    start_date <- as.Date(c("2020-04-19", "2020-05-10", "2020-05-21", "2020-06-04", "2020-06-18",
                            "2020-07-02", "2020-07-18", "2020-07-31", "2020-11-11", "2020-11-28",
                            "2020-12-10", "2020-12-23", "2021-01-06", "2021-01-20", "2021-02-05",
                            "2021-02-21", "2021-03-02", "2021-03-16", "2021-04-05", "2021-04-16",
                            "2021-04-27", "2021-05-15", "2021-05-27", "2021-06-10", "2021-06-24",
                            "2021-07-12", "2021-07-25", "2021-08-05", "2021-08-17", "2021-09-01",
                            "2021-09-14", "2021-10-01", "2021-10-16", "2021-11-01", "2021-11-11",
                            "2021-11-26", "2021-12-09", "2021-12-23", "2022-01-04", "2022-01-20",
                            "2022-02-03", "2022-02-19", "2022-03-04")),
    end_date <- c(start_date[-1] - 1, as.Date("2022-03-08")))
  }

  else if (country == "DK"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2020-12-22", "2021-01-07", "2021-01-20", "2021-02-17",
                              "2021-03-03", "2021-03-17", "2021-04-14")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-04-16")))
  }
  
  else if (country == "EE"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2021-04-20", "2021-05-06", "2021-05-25", "2021-06-23", 
                              "2021-07-08", "2021-07-26", "2021-08-10")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-08-23")))
  }
  
  else if (country == "FR"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2020-12-21", "2021-01-07", "2021-01-20", "2021-02-17",
                              "2021-03-03", "2021-03-17", "2021-04-14")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-04-15")))
  }
  
  else if (country == "PT"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2020-12-22", "2021-01-08", "2021-01-20", "2021-02-17",
                              "2021-03-03", "2021-03-17", "2021-04-15")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-04-22")))
  }
  
  else if (country == "ES"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2020-12-21", "2021-01-07", "2021-01-20", "2021-02-17",
                              "2021-03-03", "2021-03-17", "2021-04-14")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-04-19")))
  }
  
  else if (country == "IT"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2020-12-21", "2021-01-07", "2021-01-20", "2021-02-17",
                              "2021-03-03", "2021-03-17", "2021-04-14")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-04-23")))
  }
  
  else if (country == "HR"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2021-04-20", "2021-05-06", "2021-05-25", "2021-06-22", 
                              "2021-07-07", "2021-07-23", "2021-08-10")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-08-18")))
  }

  else if (country == "FI"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2021-01-22", "2021-02-18", "2021-03-04", "2021-03-18", 
                              "2021-04-15", "2021-04-29", "2021-05-13")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-05-18")))
  }
  
  else if (country == "HU"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2021-04-25", "2021-06-23", 
                              "2021-07-13", "2021-07-23", "2021-08-10",
                              "2021-08-24", "2021-09-07")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-09-14")))
  }
  
  else if (country == "LT"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2021-01-25", "2021-02-18", "2021-03-04", "2021-03-18", 
                              "2021-04-15", "2021-04-29", "2021-05-13")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-05-17")))
  }
  
  else if (country == "SK"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2021-04-20", "2021-05-06", "2021-05-25", "2021-06-22", 
                              "2021-07-07", "2021-07-23", "2021-08-10")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-08-16")))
  }
  
  else if (country == "SI"){
    comix_date_ranges <- data.frame(
      start_date <- as.Date(c("2021-03-04", "2021-03-18", "2021-04-01",
                              "2021-04-15", "2021-04-29", "2021-05-13","2021-06-02")),
      end_date <- c(start_date[-1] - 1, as.Date("2021-06-09")))
  }
  
return(comix_date_ranges)
}

