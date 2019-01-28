library(ggplot2)
library(rgdal)
library(maptools)
library(dplyr)
library(here)

#setwd("O:/PhD/Data/Portugal 2018/paper")

kml_to_df <- function(file){
  require(rgdal)
  require(ggplot2)
  ogr.file <- readOGR(file)
  ogr.file@data$id <- rownames(ogr.file@data)
  polygon <- fortify(ogr.file, region = "id")
  df <- merge(polygon, ogr.file@data, by = "id")
  return(df)
}

load(file = here("Data", "Input", "data.rda"))

# Portugal Map


Portugal.df <- kml_to_df(here("Data", "Input", "Iberian_Penin.shp"))

Portugal.gg <- ggplot(data = Portugal.df) +
  geom_path(aes(x=long, y=lat), color = "black") + 
  xlim(-9.5,-6) + ylim(37, 42.1) +
  coord_equal(ratio = 1) +
  theme_classic() +
  geom_point(data = subset, aes(x = Longitude, y = Latitude, 
                                color = Sub_Spec, shape = Section, size = 3)) +
  scale_shape_manual(values = c(15:17))
  
print(Portugal.gg)
theme(legend.position = "none", title = element_blank(),
      axis.text = element_blank())

# Points of trees
subset. <- subset[,c("ID", "Section", "Sub_Spec", "Group", "Location", "Latitude", "Longitude")]
subset.$Sub_Spec <- factor(subset.$Sub_Spec)

subset. <- subset. %>%
  group_by(Group) %>%
  mutate(mean.lat = mean(Latitude), mean.long = mean(Longitude)) %>%
  group_by(Group, Sub_Spec) %>%
  mutate(n = n_distinct(ID))

lat_lon_transform <- function(x, variable, factor, offset){
  names(offset) <- levels(x[,factor])
  new <- as.vector(x[,variable] + sapply(x[,factor], function(x) offset[x]))
  x$new <- new[,variable]
  names(x)[names(x) == "new"] <- paste("new", variable, sep = ".")
  return(x)
}

subset_ <- lat_lon_transform(x = subset., variable = "mean.lat", factor = "Sub_Spec", 
                             offset = .15 * c(0,1,-1,-1,1,0))
subset_ <- lat_lon_transform(x = subset_, variable = "mean.long", factor = "Sub_Spec", 
                             offset = .15 * c(.5,.5,-.5,.5,-.5,-.5))

subset_ <- lat_lon_transform(x = subset_, variable = "new.mean.lat", factor = "Group", 
                             offset = .05 * c(0,-.5,0,0,0,0,0,0,-.5,0,0,0,0,0,1.5))
subset_ <- lat_lon_transform(x = subset_, variable = "new.mean.long", factor = "Group", 
                             offset = .05 * c(0,.5,0,0,0,0,0,0,-1.2,0,0,0,0,0,.5))

# Map of Samples by location

Portugal.gg <- ggplot(data = Portugal.df) +
  geom_path(aes(x=long, y=lat), color = "black") + 
  xlim(-9.5,-6) + ylim(36.8, 42.2) +
  coord_equal(ratio = 1) +
  theme_bw() +
  geom_point(data = subset_, aes(x = new.new.mean.long, y = new.new.mean.lat, 
                                color = Sub_Spec, shape = Section, size = factor(n))) +
  scale_shape_manual(values = c(15:17)) +
  scale_size_manual(values = c(rep(2,3),rep(3,3),rep(4,8)),breaks = c(3,5,7), labels = c("< 3","4 - 6","> 7")) +
  labs(x = "Longitude", y = "Latitude", size = "# of Trees", shape = "Quercus Section", color = "Species")

ggsave("O:/PhD/Data/Portugal 2018/paper/Sample_map.png", plot = Portugal.gg, device = "png", 
       width = 15, height = 15, units = c("cm"), dpi = 600)
ggsave("O:/PhD/Data/Portugal 2018/paper/Sample_map.svg", plot = Portugal.gg, device = "svg", 
       width = 15, height = 15, units = c("cm"), dpi = 600)

library(RStoolbox)

elev <- readRDS(here("Data", "Output", "elevation_raster.rds"))

elevation.gg <- ggR(elev, geom_raster = TRUE) + 
  scale_fill_gradientn(colours = terrain.colors(100), name = "elevation", na.value = "white") +
  geom_path(data = Portugal.df, aes(long, lat), color = "black") +
  xlim(-9.5,-6) + ylim(36.8, 42.2)

ggsave(here("R", "figures", "elevation.png"), plot = elevation.gg, device = "png", 
            width = 15, height = 15, units = c("cm"), dpi = 600)

mean_anual_temp <- readRDS(here("Data", "Output", "mean_anual_temp_raster.rds"))

mean_anual_temp.gg <- ggR(mean_anual_temp, geom_raster = TRUE) + 
  scale_fill_gradientn(colours = c("blue", "green", "yellow", "red"), name = "temperature", na.value = "white") +
  geom_path(data = Portugal.df, aes(long, lat), color = "black") +
  xlim(-9.5,-6) + ylim(36.8, 42.2)

april_temp <- readRDS(here("Data", "Output", "april_temp_raster.rds"))

april_temp.gg <- ggR(april_temp, geom_raster = TRUE) + 
  scale_fill_gradientn(colours = c("blue", "green", "yellow", "red"), name = "temperature", na.value = "white") +
  geom_path(data = Portugal.df, aes(long, lat), color = "black") +
  xlim(-9.5,-6) + ylim(36.8, 42.2)

ggsave(here("R", "figures", "mean_anual_temp.png"), plot = mean_anual_temp.gg, device = "png", 
            width = 15, height = 15, units = c("cm"), dpi = 600)

ggsave(here("R", "figures", "april_temp.png"), plot = april_temp.gg, device = "png", 
       width = 15, height = 15, units = c("cm"), dpi = 600)

# precip WC
mean_anual_prec <- readRDS(here("Data", "Output", "mean_anual_prec_raster.rds"))

mean_anual_prec.gg <- ggR(mean_anual_prec, geom_raster = TRUE) + 
  scale_fill_gradientn(colours = c("#CCE5FF","#66B2FF","#000099"), name = "precipitation", na.value = "white") +
  geom_path(data = Portugal.df, aes(long, lat), color = "black") +
  xlim(-9.5,-6) + ylim(36.8, 42.2)

april_prec <- readRDS(here("Data", "Output", "april_prec_raster.rds"))

april_prec.gg <- ggR(april_prec, geom_raster = TRUE) + 
  scale_fill_gradientn(colours = c("#CCE5FF","#66B2FF","#000099"), name = "precipitation", na.value = "white") +
  geom_path(data = Portugal.df, aes(long, lat), color = "black") +
  xlim(-9.5,-6) + ylim(36.8, 42.2)

ggsave(here("R", "figures", "mean_anual_prec.png"), plot = mean_anual_prec.gg, device = "png", 
       width = 15, height = 15, units = c("cm"), dpi = 600)

ggsave(here("R", "figures", "april_prec.png"), plot = april_prec.gg, device = "png", 
       width = 15, height = 15, units = c("cm"), dpi = 600)

# srad WC
mean_anual_srad <- readRDS(here("Data", "Output", "mean_anual_srad_raster.rds"))

mean_anual_srad.gg <- ggR(mean_anual_srad, geom_raster = TRUE) + 
  scale_fill_gradientn(colours = c("yellow","purple"), name = "solar radiation", na.value = "white") +
  geom_path(data = Portugal.df, aes(long, lat), color = "black") +
  xlim(-9.5,-6) + ylim(36.8, 42.2)

april_srad <- readRDS(here("Data", "Output", "april_srad_raster.rds"))

april_srad.gg <- ggR(april_srad, geom_raster = TRUE) + 
  scale_fill_gradientn(colours = c("yellow","purple"), name = "april solar radiation", na.value = "white") +
  geom_path(data = Portugal.df, aes(long, lat), color = "black") +
  xlim(-9.5,-6) + ylim(36.8, 42.2)

ggsave(here("R", "figures", "mean_anual_srad.png"), plot = mean_anual_srad.gg, device = "png", 
       width = 15, height = 15, units = c("cm"), dpi = 600)

ggsave(here("R", "figures", "april_srad.png"), plot = april_srad.gg, device = "png", 
       width = 15, height = 15, units = c("cm"), dpi = 600)

##Precip map Portugal paper

prec.df <- readRDS(here("Data", "Output", "prec_data_df.rds"))

mean_yearly<-ddply(prec.df, .(ID),summarise,
                   mean_precip = mean(precipitation),
                   latitude = mean(latitude),
                   longitude = mean(longitude))
mean_yearly <- prec.df %>%
  filter(Year >= 1970) %>%
  group_by(ID) %>%
  mutate(mean_precip = mean(precipitation))

mean_yearly_prec.gg <- ggplot(mean_yearly, aes(longitude, latitude)) +
  geom_raster(aes(fill = mean_precip),interpolate = F) +
  scale_fill_gradientn(colours = c("#CCE5FF","#66B2FF","#000099")) +
  geom_path(data = Portugal.df, aes(long, lat), color = "black") +
  xlim(-9.5,-6) + ylim(36.8, 42.2) +
  coord_equal(ratio = 1)

mean_april <- prec.df %>%
  filter(Year >= 1970, Month == "April") %>%
  group_by(ID) %>%
  mutate(mean_precip = mean(precipitation))

mean_april_prec.gg <- ggplot(mean_april, aes(longitude, latitude)) +
  geom_raster(aes(fill = mean_precip),interpolate = F) +
  scale_fill_gradientn(colours = c("#CCE5FF","#66B2FF","#000099")) +
  geom_path(data = Portugal.df, aes(long, lat), color = "black") +
  xlim(-9.5,-6) + ylim(36.8, 42.2) +
  coord_equal(ratio = 1)
