setwd("O:/PhD/Data/Portugal 2018")
library(ggplot2)
library(rgdal)
library(maptools)

#Portugal Map

Portugal <- readOGR("countries/Iberian_Penin.shp")
Portugal@data$id<-rownames(Portugal@data)
Portugal.poly<-fortify(Portugal, region = "id", sort = F)

Portugal.df<-merge(Portugal.poly,Portugal@data,by="id")

Portugal.gg <- ggplot(data = Portugal.df, aes(x=long, y=lat, group = group, fill = SOVEREIGNT)) +
  geom_polygon() + xlim(-9.5,-6) + ylim(36, 44) +
  scale_fill_hue(l = 40) +
  coord_equal(ratio = 1) +
  theme_classic()
theme(legend.position = "none", title = element_blank(),
      axis.text = element_blank())

print(Portugal.gg)
