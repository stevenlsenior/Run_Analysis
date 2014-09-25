library(plotKML)

# GPX files downloaded from Runkeeper
files <- dir(pattern = "\\.gpx")

# Consolidate routes in one drata frame
index <- c()
latitude <- c()
longitude <- c()
for (i in 1:length(files)) {
  
  route <- readGPX(files[i])
  location <- route$tracks[[1]][[1]]
  ## Skip routes not in range 
    ## London: -0.10 < lon < -0.20 
    ## Reading: -1.00 < lon < -0.80
    ## Wallingford: -1.20 < lon < -1.00
  if(location$lon < -0.20 || location$lon > -0.10){
    next
  }
  else{
  index <- c(index, rep(i, dim(location)[1]))
  latitude <- c(latitude, location$lat)
  longitude <- c(longitude, location$lon)
  }
}
routes <- data.frame(cbind(index, latitude, longitude))

# Map the routes
ids <- unique(index)
plot(routes$longitude, routes$latitude, type="n", axes=FALSE, xlab="", ylab="", main="", asp=1)
for (i in 1:length(ids)) {
  currRoute <- subset(routes, index==ids[i])
  lines(currRoute$longitude, currRoute$latitude, col="#c51b8a")
}