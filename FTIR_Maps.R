
library(ggplot2)
library(rgdal)
library(maptools)

# Portugal Map
setwd("O:/PhD/Data/Portugal 2018")

Portugal <- readOGR("countries/Iberian_Penin.shp")
Portugal@data$id<-rownames(Portugal@data)

Portugal.poly<-fortify(Portugal, region = "id", sort = F)

Portugal.df<-merge(Portugal.poly, Portugal@data, by="id")

Portugal.gg <- ggplot(data = Portugal.df) +
  geom_path(aes(x=long, y=lat), color = "black") + 
  xlim(-9.5,-6) + ylim(37, 42.1) +
  coord_equal(ratio = 1) +
  theme_classic() +
  geom_point(data = subset, aes(x = Longitude, y = Latitude, 
                                color = Sub_Spec, shape = Sub_Spec, size = 4)) +
  scale_shape_manual(values = c(0:2,4:6))
  
print(Portugal.gg)
theme(legend.position = "none", title = element_blank(),
      axis.text = element_blank())

# Points of trees
setwd("O:/PhD/Data/Portugal 2018/paper")

load(file = "data.rda")

ggplot(data = subset) +
  geom_point(data = subset, aes(x = Longitude, y = Latitude, color = Sub_Spec))
