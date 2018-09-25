setwd("O:/PhD/Data/Portugal 2018")
#Reading List of Weather Stations over Portugal
f <- readLines("Climate Data/rede_seleccao_Meteorologica.csv")
stations<- read.csv(text = f, 
                    header = T, 
                    skip = 3, 
                    nrows = length(f)-9)
#Rename Variables
colnames(stations)<-colnames(stations)[-1]
colnames(stations)[3:4]<-c("Latitude","Longitude")
stations<-stations[,-23]

ggplot(data=stations, aes(Longitude, Latitude))+
  geom_point()

tmp <- readLines("Climate Data/Precip_Anual_01.csv")
Precip.anual.01 <- read.csv(text = tmp[-4], 
                            header = T,
                            skip = 2,
                            nrows = length(tmp)-7)
colnames(Precip.anual.01)
#drop<-c(grep("X", colnames(Precip.anual.01)),100)
Precip.anual.01 <- Precip.anual.01[,-c(seq.int(3,99,2),100)]

tmp <- readLines("Climate Data/Precip_monthly_01.csv")
Precip.monthly.01 <- read.csv(text = tmp[-4], 
                            header = T,
                            skip = 2,
                            nrows = length(tmp)-9)
Precip.monthly.01 <- Precip.monthly.01[,-c(seq.int(3,99,2),100)]

col.names<-colnames(Precip.monthly.01)
#Reverse
col.names[2:50] <- sapply(col.names[2:50], function(x) paste(rev(substring(x,1:nchar(x),1:nchar(x))),collapse="") )

#Split into station code only
col.names[2:50] <- sapply(col.names[2:50],function(x) sub("\\.","/",substring(x,last = regexpr("\\.\\.",x)-1,first = 2 )))

#Reverse
col.names[2:50] <- sapply(col.names[2:50], function(x) paste(rev(substring(x,1:nchar(x),1:nchar(x))),collapse="") )

#col.names<-colnames(Precip.monthly.01)
#col.names[2:50]<-sapply(col.names[2:50], function(x) sub("\\.","/",substring(x,regexpr("\\.\\.",x)+2,last = nchar(x)-1 )))
colnames(Precip.monthly.01)<-col.names

#drop<-c(grep("X", colnames(Precip.monthly.01)),100)


#gsub("\\.","e", "LINHÓ..21A.09G.") #String replacement

sub("\\.","/",substring(string,regexpr("\\.\\.",string)+2,last = nchar(string)-1 )) #Convert colname into station-code
sapply(c(2:50),function(x) sum(is.na(Precip.monthly.01[920:1040,x]))) #count NAs in columns

not.full.na<-which(!sapply(c(1:50),function(x) sum(is.na(Precip.monthly.01[920:1040,x]))) == 121, arr.ind = T) #All stations with data from the last 10 years No full NA columns

library(rgdal)
#Lito and soil

file <- "Climate Data/doc.kml"
Lito <- readOGR(file, "lito")
Lito@data$id<-rownames(Lito@data)
Lito.poly<-fortify(Lito, region = "id")

Lito.df<-merge(Lito.poly,Lito@data,by="id")

Lito.gg <- ggplot(data = Lito.df, aes(x=long, y=lat, group = group,fill = Name)) +
  geom_polygon()  +
  scale_fill_hue(l = 40) +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank())

print(Lito.gg)

#Portugal Map
file <- "Portugal_shapefile/"
World.shp<-readOGR("Countries/CNTR_BN_01M_2016_3035.shp")
World.shp@data$id <-rownames(World.shp@data)
World.poly<-fortify(World.shp,region = "id", sort = FALSE)

World.df<-merge(World.poly,Portugal.shp@data,by="id", sort = FALSE)
Portugal.df<-subset(World.df,ADMIN == "Portugal")

World.gg <- ggplot(data = Portugal.df, aes(x=long, y=lat)) +
  geom_polygon()  +
  #scale_fill_hue(l = 40) +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank())
print(World.gg)

Portugal <- readOGR("countries/Iberian_Penin.shp")
Portugal@data$id<-rownames(Portugal@data)
Portugal.poly<-fortify(Portugal, region = "id", sort = F)

Portugal.df<-merge(Portugal.poly,Portugal@data,by="id")

Portugal.gg <- ggplot(data = Portugal.df, aes(x=long, y=lat, group = group,fill = SOVEREIGNT)) +
  geom_polygon()  +
  scale_fill_hue(l = 40) +
  coord_equal() 
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank())

print(Portugal.gg)


#Rain data from paper
setwd("./Climate Data/pt02_ascii/mensal")
library(stringr)
library(plyr)
library(ggplot2)

files <- list.files(pattern = "*PT_mensal*")
month<-c("January","February","March","April","Mai","June","Juli","August","September","October","November","December")

read.files<-function(file, month){
  f <- readLines(file)
  f<-str_replace_all(f," ",",")
  tmp <- read.csv(text = f, header = F)
  tmp$V2<-rep(c(1950:2003), each = 258)
  tmp<-tmp[-seq.int(1,13932,258),]
  tmp<-tmp[,c(2:4,7,8)]
  tmp$V3<-month
  colnames(tmp)<-c("Year", "Month", "latitude", "longitude", "precipitation")
  return(tmp)
}
l<-lapply(c(1:12),function(x) read.files(files[x],month[x]))

data.df<-do.call(rbind,l)

data.df$ID<-paste(as.character(data.df$latitude),as.character(data.df$longitude),sep = "_")
data.df$ID<-factor(data.df$ID)
data.df$Year<-factor(data.df$Year)
data.df$Month<-factor(data.df$Month)

levels(data.df$ID) <- sapply(c(1:257),function(x) paste("St",formatC(x, width=3, flag="0"),sep="_"))

mean_yearly<-ddply(data.df, .(ID),summarise,
      mean_precip = mean(precipitation),
      latitude = mean(latitude),
      longitude = mean(longitude))

ggplot(mean_yearly, aes(longitude, latitude))+
  geom_raster(aes(fill = mean_precip),interpolate = T)+
  scale_fill_gradientn(colours = c("#CCE5FF","#66B2FF","#000099"))+
  geom_point(data=subset(Portugal, Species == "robur" | Species == "rotundifolia" | Species == "suber" | Species == "faginea" | Species == "coccifera" | Species == "lusitanica"), aes(Longitude, Latitude, colour = Species))+
  scale_color_brewer(palette = "Set1")
  
library(raster)

dfr<-rasterFromXYZ(mean_yearly)
crs(dfr)<-CRS("+init=EPSG:4326")

load("FTIR/samples.df.rda")
coordinates(samples.df)<-~Longitude+Latitude
crs(samples.df)<-CRS("+init=EPSG:4326")

data <- data.frame(coordinates(samples.df),
                   samples.df$SID, 
                   extract(dfr, samples.df))
data[is.na(data$mean_precip),4]<-59.61478 # Fill in NAs with closest Gridvalue

#WorldClim

coords = matrix(c(-10.215,42.188,
                  -10.215,36.818,
                  -6.088,36.818,
                  -6.088,42.188), 
                ncol = 2, byrow = TRUE)
P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), 
                      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

Port <- getData("GADM", country="Portugal", level = 0)
setwd("O:/PhD/Data/WorldClim")
fn <- "wc2.0_30s_tavg_04.tif"
fn <- "wc2.0_30s_tavg_05.tif"
r2<-raster(fn)
rr<-mask(r1,Ps1)

files<-list.files()
files[grepl(".tif", files)]

l<-lapply(files[grepl(".tif", files)], function(x){
  r<-raster(x)
  r<-crop(r, Ps1)
  return(r)
})
mean_anual_temp<-overlay(l[[1]],l[[2]],l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]],l[[10]],l[[11]],l[[12]],fun=mean)

data <- data.frame(coordinates(samples.df),
                   samples.df$SID, 
                   extract(mean_anual_temp, samples.df))
s<-is.na(data$extract.mean_anual_temp..samples.df.)
data[s,4]<-15.625


#Elevation data
t<-getData("alt",country="PRT",mask=FALSE)
elev<-t[[1]]
d <- data.frame(coordinates(samples.df),
                   samples.df$SID, 
                   extract(elev, samples.df))

data$elevation<-d$extract.elev..samples.df.
