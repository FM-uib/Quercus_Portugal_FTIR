setwd("O:/PhD/Data/port climate")
files <- list.files("prec/")

read_pt_weather_data <- function(files){
  tmp <- readLines(paste0("prec/", files))
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

prec1 <- read_pt_weather_data(files[1])
prec2 <- read_pt_weather_data(files[1])
prec <- cbind(prec2,prec1[,-1])

prec <- prec[29:89,] # only march and april

hist(sapply(c(2:92),function(x) sum(is.na(prec[,x]))), breaks = c(-1:70)) # plot no of NAs

prec <- prec[,-(which(sapply(c(1:92),function(x) sum(is.na(prec[,x]))) > 5, arr.ind = T))]

prec[is.na(prec)] <- 0

sum_prec <- prec[,-1] %>%
  summarise_all(., sum)

tsum <- as.data.frame(t(sum_prec))
tsum$ID <- row.names(tsum)
tsum$m_prec <- tsum$V1/61

load("Stations.rda")

tsum$lat <- stations[tsum$ID,"Latitude"]
tsum$lon <- stations[tsum$ID,"Longitude"]

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

# mnearest neighbor

coordinates(tsum)<-~lon+lat
crs(tsum)<-CRS("+init=EPSG:4326")
pD_trees<-pointDistance(samples.df, tsum, lonlat = TRUE)
library(Rfast)
ind <- rowMins(pD_trees)

subset$april_prec_WS <- sapply(ind, function(x) tsum$m_prec[x])

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
