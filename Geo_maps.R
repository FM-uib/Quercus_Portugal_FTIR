library(rgdal)
library(here)
library(ggplot2)

#Lito and soil

Lito <- readOGR(here("Data","Input","Lito.kml"))
Lito@data$id<-rownames(Lito@data)
Lito.poly<-fortify(Lito, region = "id")

Lito.df<-merge(Lito.poly,Lito@data,by="id")



print(Lito.gg)


kml_to_df <- function(file){
  require(rgdal)
  require(ggplot2)
  ogr.file <- readOGR(file)
  ogr.file@data$id <- rownames(ogr.file@data)
  polygon <- fortify(ogr.file, region = "id")
  df <- merge(polygon, ogr.file@data, by = "id")
  return(df)
}

between <- function(search_str, x){
  sub <- substring(x,
            regexpr(search_str, x) + str_length(search_str))
  sub2 <- substring(sub, regexpr("<td>", sub) + 4, regexpr("</td>", sub)-1)
  return(sub2)
}

search_str <- c("<td>COD_DIGITA_1</td>", "<td>UNIDADE_CA_1</td>", "<td>GRUPO_1</td>", "<td>ZONA_1</td>")

carta_geo.df$COD_DIGITA <- as.factor(unlist(lapply(carta_geo.df$Description, 
                                                   function(x) between(search_str[1],x))))
carta_geo.df$UNIDADE <- as.factor(unlist(lapply(carta_geo.df$Description, 
                                                function(x) between(search_str[2],x))))
carta_geo.df$GRUPO <- as.factor(unlist(lapply(carta_geo.df$Description, 
                                              function(x) between(search_str[3],x))))
carta_geo.df$ZONA <- as.factor(unlist(lapply(carta_geo.df$Description, 
                                             function(x) between(search_str[4],x))))

saveRDS(Lito.df, here("Data","Output","Lito.rds"))
saveRDS(carta_geo.df, here("Data","Output","carta_geo.rds"))

carta_geo.df <- readRDS(here("Data","Output","carta_geo.rds"))
Lito.df <- readRDS(here("Data","Output","Lito.rds"))

carta_geo.gg <- ggplot(data = carta_geo.df, aes(x=long, y=lat, group = group,fill = UNIDADE_CA)) +
  geom_polygon()  +
  scale_fill_hue(l = 40) +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank())


search_str <- c("<td>COMP_LIT</td>", "<td>PER_GEOL</td>", "<td>DESIGNA</td>")

Lito.df$COMP_LIT <- as.factor(unlist(lapply(Lito.df$Description, 
                                                   function(x) between(search_str[1],x))))

Lito.gg <- ggplot(data = Lito.df, aes(x=long, y=lat, group = group,fill = Name)) +
  geom_polygon()  +
  scale_fill_hue(l = 40) +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank())