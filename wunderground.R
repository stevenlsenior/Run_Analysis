wunderground <- function(date, celcius = TRUE){
  # Convert date to POSIXct format
  date <- as.POSIXct(date)
  
  # Initialise data vectors
  temp_mean <- c()
  temp_max <- c()
  temp_min <- c()
  rain <- c()
  
  # Fixed elements of wunderground.com URL and patterns for searching HTML
  path_base <- "http://www.wunderground.com/history/airport/EGLL/"
  path_end <- "DailyHistory.html"
  temp_type <- "Celcius"
  pattern_temp <- ">(\\-*[0-9]+)</span>&nbsp;&deg;C"
  pattern_rain <- ">([0-9]+\\.[0-9]+)</span>&nbsp;mm"
  
  # Loop through dates and retreive mean, max and min temps and precipitation
  for (l in 1:length(date)){
    path_date <- strftime(date[l], format = "%Y/%m/%d/")
    path <- paste(path_base, path_date, path_end, sep = "")
    page <- readLines(path)
      
    temp_mean_line <- grep("Mean Temperature", page) + 2
    temp_max_line <- grep("Max Temperature", page) + 2
    temp_min_line <- grep("Min Temperature", page) + 2
    rain_line <- grep(">([0-9]+.[0-9]+)</span>&nbsp;mm</span", page)
    
    # Determine if data is celcius or farenheit
    if(grepl(pattern_temp, page[temp_mean_line]) == FALSE){
      temp_type <- "Farenheit"
      pattern_temp <- ">(\\-*[0-9]+)</span>&nbsp;&deg;F"
    }
    
    temp_mean_index <- regexec(pattern_temp, page[temp_mean_line])
    temp_max_index <- regexec(pattern_temp, page[temp_max_line])
    temp_min_index <- regexec(pattern_temp, page[temp_min_line])
    rain_index <- regexec(pattern_rain, page[rain_line])
        
    temp_mean <- c(temp_mean, as.numeric(regmatches(page[temp_mean_line], temp_mean_index)[[1]][2]))
    temp_max <- c(temp_max, as.numeric(regmatches(page[temp_max_line], temp_max_index)[[1]][2]))
    temp_min <- c(temp_min, as.numeric(regmatches(page[temp_min_line], temp_min_index)[[1]][2]))
    rain <- c(rain, as.numeric(regmatches(page[rain_line], rain_index)[[1]][2]))
  }
  
  # Convert to deg C if celcius equal to TRUE
  if(celcius == TRUE & temp_type == "Farenheit"){
   temp_min <- (temp_min - 32) * 5 / 9
   temp_max <- (temp_max - 32) * 5 / 9
   temp_mean <- (temp_mean - 32) * 5 / 9  
   temp_type <- "Celcius"
  }
  
  # Convert to deg F if celcius equal to FALSE
  else if (celcius == FALSE & temp_type == "Celcius"){
    temp_min <- (temp_min * 9 / 5) + 32
    temp_max <- (temp_max * 9 / 5) + 32
    temp_mean <- (temp_mean * 9 / 5) + 32
    temp_type <- "Farenheit"
  }
  data <- data.frame(temp_min, temp_max, temp_mean, rain)
  return(data)
}
