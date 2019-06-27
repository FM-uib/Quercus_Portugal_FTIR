library(here)
library(raster)
library(sp)
library(gstat)
library(dplyr)
library(lattice)
source(here("R","functions","02_env.R"))
load(here("Data", "Input", "environmental variables", "Stations.rda"))
data = readRDS(file = here("Data", "Input", "data_mean.rds"))

# Load elevation data
# To download from source use following command
# t <- getData("alt",country="PRT",mask=FALSE) 
# elevation <- t[[1]]

elevation = raster(here("Data", "Input","environmental variables","elevation", "PRT1_alt.grd"))
data$elevation = extract(elevation, data[,c("Longitude","Latitude")])

# Load Precipitation Data
prec <- lapply(list.files(here("Data", "Input", "environmental variables", "prec"), full.names = T), 
               function(x) read_pt_weather_data(x))
prec <- do.call(cbind, prec)
prec <- prec[,!duplicated(colnames(prec))]
prec$DATA <- as.Date(as.character(prec$DATA), format = "%d/%m/%Y")
prec$Date_num <- as.numeric(prec$DATA)

# Load Temperature Data
temp <- read_pt_weather_data(list.files(here("Data", "Input", "environmental variables", "temp"), full.names = T))
temp$DATA <- as.Date(as.character(temp$DATA), format = "%d/%m/%Y")
temp$Date_num <- as.numeric(temp$DATA)

# Load Srad Data
srad <- read_pt_weather_data(list.files(here("Data", "Input", "environmental variables", "srad"), full.names = T))
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
