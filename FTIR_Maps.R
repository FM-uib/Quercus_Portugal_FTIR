library(ggplot2)
library(rgdal)
library(maptools)
library(dplyr)

setwd("O:/PhD/Data/Portugal 2018/paper")

load(file = "data.rda")

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

ggplot(data = subset_) +
  geom_point(data = subset_, aes(x = new.mean.long, y = new.mean.lat, color = Sub_Spec, size = n))

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

ggplot(data = subset_) +
  geom_point(data = subset_, aes(x = new.new.mean.long, y = new.new.mean.lat, 
                                 color = Sub_Spec, shape = Section, size = factor(n))) +
  coord_equal(ratio = 1) +
  scale_size_manual(values = c(rep(2,3),rep(3,3),rep(4,8)),breaks = c(3,5,7), labels = c("a","b","c"))
