library(plotKML)

# GPX files downloaded from Runkeeper
files <- dir(pattern = "\\.gpx")

# Initialise variables of interest
date <-vector()
class(date) <- "POSIXct"
start_time <- vector()
class(start_time) <- "POSIXct"
end_time <-vector()
class(end_time) <- "POSIXct"
duration <-vector()
dist <- c()
elevation <- c()
avg_pace <-c()
temp_mean <- c()
temp_max <- c()
temp_min <- c()
rain <- c()

# Collect data for each run
for (i in 1:length(files)) {
  
  # Read in files
  route <- readGPX(files[i])
  location <- route$tracks[[1]][[1]]
  
  # Limit to runs in London
  if(location$lon < -0.20 || location$lon > -0.10){
    next
  }
  else{
    # Append values for this track
    date <- c(date, as.POSIXct(location$time[1], format = "%Y-%m-%d", tz = "GMT"))
    start_time <- c(start_time, as.POSIXct(location$time[1], format = "%Y-%m-%dT%H:%M:%S", tz = "GMT"))
    end_time <- c(end_time, as.POSIXct(location$time[length(location$time)], format = "%Y-%m-%dT%H:%M:%S", tz = "GMT"))
    duration <- c(duration, (as.numeric(end_time[length(end_time)] - start_time[length(start_time)])))
    
    # Calculate route length using Haversin formula
    track_dist <- 0
    for(j in 2:length(location$lon)){
      lon1 <- (location$lon[j-1])*(pi/180)
      lon2 <- (location$lon[j])*(pi/180)
      lat1 <- (location$lat[j-1])*(pi/180)
      lat2 <- (location$lat[j])*(pi/180)
      
      dlat <- lat2 - lat1
      dlon <- lon2 - lon1
      
      a <- (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
      c <- 2 * atan2(sqrt(a), sqrt(1-a))
      
      # Km - R = 6367; Miles - R = 3956
      R <- 6367
      d <- R * c
      track_dist <- track_dist + d
    }
    dist <- c(dist, track_dist)
    elevation <- c(elevation, as.numeric(location$ele[length(location$ele)]) - as.numeric(location$ele[1]))
  }
}

avg_pace <- duration / dist

# Scrape weather data from weatherunderground.com
path_base <- "http://www.wunderground.com/history/airport/EGLL/"
path_end <- "DailyHistory.html"
pattern_temp <- ">(\\-*[0-9]+)</span>&nbsp;&deg;C"
pattern_rain <- ">([0-9]+\\.[0-9]+)</span>&nbsp;mm"

for (l in 1:length(date)){
  path_date <- strftime(date[l], format = "%Y/%m/%d/")
  path <- paste(path_base, path_date, path_end, sep = "")
  page <- readLines(path) 
  
  temp_mean_line <- grep("Mean Temperature", page) + 2
  temp_max_line <- grep("Max Temperature", page) + 2
  temp_min_line <- grep("Min Temperature", page) + 2
  rain_line <- grep(">([0-9]+.[0-9]+)</span>&nbsp;mm</span", page)
  
  temp_mean_index <- regexec(pattern_temp, page[temp_mean_line])
  temp_max_index <- regexec(pattern_temp, page[temp_max_line])
  temp_min_index <- regexec(pattern_temp, page[temp_min_line])
  rain_index <- regexec(pattern_rain, page[rain_line])
  
  temp_mean <- c(temp_mean, as.numeric(regmatches(page[temp_mean_line], temp_mean_index)[[1]][2]))
  temp_max <- c(temp_max, as.numeric(regmatches(page[temp_max_line], temp_max_index)[[1]][2]))
  temp_min <- c(temp_min, as.numeric(regmatches(page[temp_min_line], temp_min_index)[[1]][2]))
  rain <- c(rain, as.numeric(regmatches(page[rain_line], rain_index)[[1]][2]))
}

# Complile variables into dataframe and return
run_data <- data.frame (date, start_time, end_time, duration, dist, avg_pace, elevation, temp_mean, temp_max, temp_min, rain)
View(run_data)