
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
subset. <- subset[,c("ID", "Sub_Spec", "Group", "Location", "Latitude", "Longitude")]
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
                             offset = .1 * c(0,1,-1,-1,1,0))
subset_ <- lat_lon_transform(x = subset_, variable = "mean.long", factor = "Sub_Spec", 
                             offset = .1 * c(.5,.5,-.5,.5,-.5,-.5))

ggplot(data = subset_) +
  geom_point(data = subset_, aes(x = new.mean.long, y = new.mean.lat, color = Sub_Spec, size = n))

  summarize(mean.lat = mean(Latitude), mean.long = mean(Longitude))



ggplot(data = subset.) +
  geom_point(data = subset., aes(x = Longitude, y = Latitude, color = Sub_Spec))

