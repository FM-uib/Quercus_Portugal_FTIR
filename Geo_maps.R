library(rgdal)
library(here)
library(ggplot2)

#Lito and soil


Lito <- readOGR(here("Data","Input","Lito.kml"))
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


function(file, layer){
  readOGR(file, layer)
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
carta_geo.gg <- ggplot(data = carta_geo.df, aes(x=long, y=lat, group = group,fill = UNIDADE_CA)) +
  geom_polygon()  +
  scale_fill_hue(l = 40) +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank())