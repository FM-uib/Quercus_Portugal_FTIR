library(here)
library(ggplot2)
library(rgdal)
library(maptools)
library(dplyr)

source(here("R", "functions", "04_plotting.R"))
data = readRDS(file = here("Data", "Output", "data_mean.rds"))

# Map

## Portugal Map
Portugal.df <- kml_to_df(here("Data", "Input", "map", "Iberian_Penin.shp"))

### Points of trees
trees <- data[,c("ID", "Section", "Sub_Spec", "Group", "Location", "Latitude", "Longitude")]
trees$Sub_Spec <- factor(trees$Sub_Spec)

trees <- trees %>%
  group_by(Group) %>%
  mutate(mean.lat = mean(Latitude), mean.long = mean(Longitude)) %>%
  group_by(Group, Sub_Spec) %>%
  mutate(n = n_distinct(ID))

trees <- lat_lon_transform(x = trees, variable = "mean.lat", factor = "Sub_Spec", 
                             offset = .15 * c(0,1,-1,-1,1,0))
trees <- lat_lon_transform(x = trees, variable = "mean.long", factor = "Sub_Spec", 
                             offset = .15 * c(.5,.5,-.5,.5,-.5,-.5))

trees <- lat_lon_transform(x = trees, variable = "new.mean.lat", factor = "Group", 
                             offset = .05 * c(0,-.5,0,0,0,0,0,0,-.5,0,0,0,0,0,1.5))
trees <- lat_lon_transform(x = trees, variable = "new.mean.long", factor = "Group", 
                             offset = .05 * c(0,.5,0,0,0,0,0,0,-1.2,0,0,0,0,0,.5))

### Map of Samples by location
library(RStoolbox)

elev <- readRDS(here("Data", "Output", "elevation_raster.rds"))
elev@file@name = here("Data", "Input", "environmental variables", "elevation", "PRT1_alt.grd")

Portugal.gg <- ggplot() +
  ggR(elev, geom_raster = TRUE, alpha = .4, ggLayer = T) + 
  scale_fill_gradientn(colours = gray.colors(100, alpha = .4), name = "elevation", na.value = "white", guide = F) +
  geom_path(data = Portugal.df, aes(x=long, y=lat), color = "black") + 
  xlim(-10,-6.3) + ylim(36.8, 42.1) +
  coord_equal(ratio = 1) +
  theme_bw() +
  geom_point(data = trees, aes(x = new.new.mean.long, y = new.new.mean.lat, 
                                 color = Sub_Spec, shape = Section, size = factor(n))) +
  scale_shape_manual(values = c(15:17)) +
  scale_size_manual(values = c(rep(2,3),rep(3,3),rep(4,8)),breaks = c(3,5,7), labels = c("< 3","4 - 6","> 7")) +
  labs(x = "Longitude", y = "Latitude", size = "# of Trees", shape = "Quercus Section", color = "Species")

ggsave("O:/PhD/Data/Portugal 2018/paper/Sample_map.png", plot = Portugal.gg, device = "png", 
       width = 15, height = 15, units = c("cm"), dpi = 600)
ggsave("O:/PhD/Data/Portugal 2018/paper/Sample_map.svg", plot = Portugal.gg, device = "svg", 
       width = 15, height = 15, units = c("cm"), dpi = 600)


plt = loadings_plot(pls_model)
ggsave("xload.png", plot = (plt), device = "png", path = here("R", "figures"), width = 20, height = 30, units = "cm", dpi = 600)

