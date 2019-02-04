setwd("O:/PhD/Data/port climate")
load("Stations.rda")

read_pt_weather_data <- function(files){
  tmp <- readLines(files)
  df <- read.csv(text = tmp[-4], 
                 header = T,
                 skip = 2,
                 nrows = length(tmp)-9)
  df <- df[,-c(seq.int(3,ncol(df),2),ncol(df))]
  tmp <- colnames(df)
  tmp[-1] <- sapply(tmp[-1], function(string) sub("\\.","/",substring(string,regexpr("\\.\\.\\d",string)+2,last = nchar(string)-1 )))
  colnames(df) <- tmp
  return(df)
}
calc_weather <- function(data, stations) {
  require(dplyr)
  tmp1 <- data[,-1] %>%
    summarise_all(., sum)
  tmp2 <- data.frame(ID = colnames(data)[-1],
                        sum = t(tmp1),
                        mean_d = t(tmp1)/nrow(data),
                        mean_m = t(tmp1)/(nrow(data)/31),
                        lat = stations[colnames(data)[-1], "Latitude"],
                        lon = stations[colnames(data)[-1], "Longitude"])
  return(tmp2)
}
nearest_neighbor <- function(data, stations, value){
  require(raster)
  require(Rfast)
  coordinates(stations) <- ~lon+lat
  crs(stations) <- CRS("+init=EPSG:4326")
  coordinates(data)<-~Longitude+Latitude
  crs(data)<-CRS("+init=EPSG:4326")
  pD <- pointDistance(data, stations, lonlat = TRUE)
  ind <- rowMins(pD)
  results <- sapply(ind, function(x) stations@data[x,value])
  return(results)
}
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

# Precipitation
prec1 <- read_pt_weather_data(list.files("prec/", full.names = T)[1])
prec2 <- read_pt_weather_data(list.files("prec/", full.names = T)[2])
prec <- cbind(prec2,prec1[,-1])

# Solar Radiation
srad <- read_pt_weather_data(list.files("srad/", full.names = T))

# Mean daily Temp
temp <- read_pt_weather_data(list.files("temp/", full.names = T))

# only select march and april of the data
prec <- prec[29:89,] 
srad <- srad[29:89,]
temp <- temp[29:89,]

hist(sapply(c(2:ncol(prec)),function(x) sum(is.na(prec[,x]))), breaks = c(-1:70)) # plot no. of NAs

# Selecting the stations with 5 or less missing days of data. 
prec <- prec[,-(which(sapply(c(1:ncol(prec)),function(x) sum(is.na(prec[,x]))) > 5, arr.ind = T))]
srad <- srad[,-(which(sapply(c(1:ncol(srad)),function(x) sum(is.na(srad[,x]))) > 5, arr.ind = T))]
temp <- temp[,-(which(sapply(c(1:ncol(temp)),function(x) sum(is.na(temp[,x]))) > 5, arr.ind = T))]

prec[is.na(prec)] <- 0
srad[,-1] <- as.data.frame(sapply(srad[,-1], NA2mean))
temp[,-1] <- as.data.frame(sapply(temp[,-1], NA2mean))

prec_calc <- calc_weather(prec, stations)
srad_calc <- calc_weather(srad, stations)
temp_calc <- calc_weather(temp, stations)

# nearest neighbor
subset$prec_mean_d_WS <- nearest_neighbor(subset, prec_calc, "mean_d")
subset$srad_mean_d_WS <- nearest_neighbor(subset, srad_calc, "mean_d")
subset$temp_mean_d_WS <- nearest_neighbor(subset, temp_calc, "mean_d")

e_data <- data.frame(ID = subset$ID,
                     prec_mean_d_MA_WS = nearest_neighbor(subset, prec_calc, "mean_d"),
                     srad_mean_d_MA_WS = nearest_neighbor(subset, srad_calc, "mean_d"),
                     temp_mean_d_MA_WS = nearest_neighbor(subset, temp_calc, "mean_d"))

# interpolation
library(akima)

prec_r <- interp(tsum$lon, tsum$lat, tsum$m_prec, linear = T, nx = 100, ny = 200)

prec_grid <- expand.grid(prec_r[[1]], prec_r[[2]])
prec_grid$z <- as.vector(prec_r[[3]])
colnames(prec_grid) <- c("lon", "lat", "precipitation")

dfr<-rasterFromXYZ(prec_grid, crs = CRS("+init=EPSG:4326"))
crs(dfr)<-CRS("+init=EPSG:4326")

load(here("Data","Input","data_WC.rda"))

load("O:/PhD/Code/Portugal 2018/Data/Input/data_WC.rda") 

samples.df <- subset
coordinates(samples.df)<-~Longitude+Latitude
crs(samples.df)<-CRS("+init=EPSG:4326")
subset$april_prec_WS <- extract(dfr, samples.df)

library(ggplot2)

ggplot(prec_grid, aes(lon, lat)) +
  geom_raster(aes(fill = precipitation)) +
  scale_fill_gradientn(colours = c("#CCE5FF","#66B2FF","#000099")) +
  coord_equal() +
  geom_point(data = tsum, aes(lon, lat)) +
  geom_point(data = subset[is.na(subset$april_prec_WS),], aes(Longitude, Latitude, color = "red"))


applied <- function(ind){
  subset$april_prec_WS <- sapply(ind, function(x) tsum$m_prec[x])
}

forloop <- function(ind){
  x = 1
  for (i in ind){
    subset$april_prec_WS[x] <- tsum$m_prec[i]
    x = x+1
  }
}
microbenchmark(applied(ind), forloop(ind), times = 10)
