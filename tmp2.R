library(here)
library(sp)
library(gstat)
library(dplyr)
library(lattice)
load(here("Data", "Input", "climate_data", "Stations.rda"))
data = readRDS(file = here("Data", "Input", "data_meaned.rds"))
data$Date[is.na(data$Date)] <- mean(subset(data, Sub_Spec == "rotundifolia")[,"Date"], na.rm = T)
source(here("R","functions","read_pt_weather_data.R"))
source(here("R","functions","krige_env.R"))
source(here("R","functions","extract_from_krige.R"))
source(here("R","functions","lapsed_temp.R"))
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

# Load Precipitation Data
prec <- lapply(list.files(here("Data","Input","climate_data","prec"), full.names = T), 
               function(x) read_pt_weather_data(x))
prec <- do.call(cbind, prec)
prec <- prec[,!duplicated(colnames(prec))]
prec$DATA <- as.Date(as.character(prec$DATA), format = "%d/%m/%Y")
prec$Date_num <- as.numeric(prec$DATA)

# Load Temperature Data
temp <- read_pt_weather_data(list.files(here("Data","Input","climate_data","temp"), full.names = T))
temp$DATA <- as.Date(as.character(temp$DATA), format = "%d/%m/%Y")
temp$Date_num <- as.numeric(temp$DATA)

# Load Srad Data
srad <- read_pt_weather_data(list.files(here("Data","Input","climate_data","srad"), full.names = T))
srad$DATA <- as.Date(as.character(srad$DATA), format = "%d/%m/%Y")
srad$Date_num <- as.numeric(srad$DATA)

# sub select date range
prec <- subset(prec, DATA > min(data$Date)-31 & DATA <= max(data$Date))
temp <- subset(temp, DATA > min(data$Date)-31 & DATA <= max(data$Date))
srad <- subset(srad, DATA > min(data$Date)-31 & DATA <= max(data$Date))

# delete stations with more than 5 missing dates of data
prec <- prec[,-(which(sapply(c(1:ncol(prec)),function(x) sum(is.na(prec[,x]))) > 5, arr.ind = T))]
srad <- srad[,-(which(sapply(c(1:ncol(srad)),function(x) sum(is.na(srad[,x]))) > 5, arr.ind = T))]
temp <- temp[,-(which(sapply(c(1:ncol(temp)),function(x) sum(is.na(temp[,x]))) > 5, arr.ind = T))]

# replace NAs with 0s (precipitation) or mean
prec[is.na(prec)] <- 0
srad[,-1] <- as.data.frame(sapply(srad[,-1], NA2mean))
temp[,-1] <- as.data.frame(sapply(temp[,-1], NA2mean))

# calculate temp on 0 masl using adiabatic lapse rate
temp[,grep(pattern = "*//*",colnames(temp), value = T)] <- sapply(grep(pattern = "*//*",colnames(temp), value = T), function(x) lapsed_temp(as.numeric(stations[x, 2])/1000, 0, temp[,x]))

# Kriging (.1 = 10 km)
prec_krig <- krige_env(prec, stations)
temp_krig <- krige_env(temp, stations)
srad_krig <- krige_env(srad, stations)

saveRDS(prec_krig, here("Data","Output","prec_krig_10km.rds"))
saveRDS(temp_krig, here("Data","Output","temp_krig_10km.rds"))
saveRDS(srad_krig, here("Data","Output","srad_krig_10km.rds"))

prec_krig = readRDS(file = here("Data","Output","prec_krig_10km.rds"))
temp_krig = readRDS(file = here("Data","Output","temp_krig_10km.rds"))
srad_krig = readRDS(file = here("Data","Output","srad_krig_10km.rds"))

env30 <- data.frame(ID = data$ID,
                  prec = extract_from_krige(data, prec_krig, length = 30),
                  temp = lapsed_temp(old_alt = 0, new_alt = data$elevation/1000,
                                     extract_from_krige(data, temp_krig, length = 30, sum = F)),
                  srad = extract_from_krige(data, srad_krig, sum = F, length = 30))
env14 <- data.frame(ID = data$ID,
                    prec = extract_from_krige(data, prec_krig, length = 14),
                    temp = lapsed_temp(old_alt = 0, new_alt = data$elevation/1000,
                                       extract_from_krige(data, temp_krig, length = 14, sum = F)),
                    srad = extract_from_krige(data, srad_krig, sum = F, length = 14))

saveRDS(env30, here("Data","Output","env_WS_kriged_30.rds"))
saveRDS(env14, here("Data","Output","env_WS_kriged_14.rds"))

levelplot(var1.pred ~ x + y | z, as.data.frame(prec_krig))

melted <- melt(prec_krig)
colnames(melted) <- c("lon","lat","date","value","sd")
dat <- data.frame(lat = sample(data$Latitude, 10), lon = sample(data$Longitude, 10), date = sample(unique(melted$date)[-c(6,7)], 10, T))
#loop over the samples
melted[melted$value < 0, 4] = 0
dat$prec <- sapply(c(1:nrow(dat)), function(x){


#extract date series out of dataframe
  tmp <- subset(melted, date>dat$date[x] & date<dat$date[x]+2)
# extract by location
  tmp <- tmp[tmp[,2] %in% round(dat[x, "lat"],1) & tmp[,1] %in% round(dat[x, "lon"],1),4]
#aggregate env stat
  sum(tmp)
})

extract_from_krige <- function(data, kriged, length = 14, sum = T){
  require(reshape2)
  melted <- melt(kriged)
  colnames(melted) <- c("lon","lat","date","value","sd")
  melted[melted$value < 0, 4] = 0
  tmp_env <- sapply(c(1:nrow(data)), function(x){
    # extract date series out of dataframe
    tmp <- subset(melted, date > data$Date[x] & date < data$Date[x] + length)
    # extract by location
    tmp <- tmp[tmp[,2] %in% round(data[x,"Latitude"],1) & tmp[,1] %in% round(data[x, "Longitude"],1),4]
    # aggregate env stat
    if(sum){
      sum(tmp)
    }else{
      mean(tmp)
    }
  })
}

# extract stack of weather data
krig <- prec_krig
krig@data<- as.data.frame(value = krig@data[,-2])
crs(krig) <- CRS("+init=EPSG:4326")
library(raster)
prec_brick <- brick(krig)

# select weather data by date selection

rnd <- function(x, dec, floor){
  if(floor){
    tmp <- floor(x * (1/dec)) / (1/dec)
  }else{
    tmp <- ceiling(x * (1/dec)) / (1/dec)
  }
  return(tmp)
}

krige_env <- function(data, stations, sel = c("DATA", "Date_num"), cell_size = .1){
  require(dplyr)
  require(reshape2)
  require(gstat)
  grid <- melt(select(data, -sel))
  grid$variable <- as.character(grid$variable)
  grid$x <- stations[grid$variable, "Longitude"]
  grid$y <- stations[grid$variable, "Latitude"]
  grid$z <- rep(data$Date_num, length(unique(grid$variable)))
  coordinates(grid) = ~x+y+z
  #grid_pred <- expand.grid(x = seq(min(grid$x),max(grid$x),cell_size),
  #                         y = seq(min(grid$y),max(grid$y),cell_size),
  #                         z = c(min(unique(grid@coords[,"z"])):max(unique(grid@coords[,"z"]))))
  #gridded(grid_pred) <- ~x+y+z
  #prec_krig <- krige(formula = value ~ 1, grid, grid_pred, model = vgm (1, "Exp", .2))
  return(grid)
}
grid$z <- rep(c(1:11),91)

grid_pred <- expand.grid(x = seq(min(grid$x),max(grid$x),.1),
                         y = seq(min(grid$y),max(grid$y),.1),
                         z = c(min(unique(grid@coords[,"z"])):max(unique(grid@coords[,"z"]))))
#c(1:11))#
gridded(grid_pred) <- ~x+y+z
prec_krig <- krige(formula = value ~ 1, grid, grid_pred, model = vgm (1, "Exp", .2))
levelplot(var1.pred ~ x + y | z, as.data.frame(prec_krig))

#playing with Automap
grid_data <- krige_env(prec[68,], stations)
aut_kri <- autoKrige(value ~ x+y+z, grid_data, grid_pred)
prec_krig <- krige(formula = value ~ 1, grid_data, grid_pred, model = vgm (1, "Exp", .2))
plot(aut_kri)

# Create Grid to Predict
data2 <- melt(prec[69:79,c(-1,-93)])
data2$variable<- as.character(data2$variable)
data2$x <- stations[data2$variable,"Longitude"]
data2$y <- stations[data2$variable,"Latitude"]
data2$z <- rep(c(1:11),91)

data3D <- data2[,-1]
coordinates(data3D) = ~x+y+z

grid3D <- expand.grid(x = seq(min(data2$x),max(data2$x),.55), 
                      y = seq(min(data2$y),max(data2$y),.35), 
                      z = c(1:11))

gridded(grid3D) = ~x+y+z

res3D <- krige(formula = value ~ 1, data3D, grid3D, model = vgm(1, "Exp", .2))
levelplot(var1.pred ~ x + y | z, as.data.frame(res3D))

# Convert to STFDF
data <- data.frame(lat = stations[colnames(prec)[-1], "Latitude"],
                   lon = stations[colnames(prec)[-1], "Longitude"])
coordinates(data) <- ~ lon + lat
crs(data)<-CRS("+init=EPSG:4326")
data <- STFDF(data, prec$DATA, data.frame(p = as.vector(as.matrix(prec[,-1]))))

v <- variogramST(p~1,data, tlags = 0:30, width = 5)
plot(v)
plot(v, all=T, wireframe = T, scales = list(arrows = F))