library(ggplot2)
setwd("O:/PhD/Data/RGIS Tut")
setwd("O:/PhD/Data/Portugal 2018")


setwd("O:/PhD/Drafts/Porto Fieldwork")
#Portugal<-read.csv("Porto_Samples.csv")
head(Portugal)

clusters <- kmeans(Portugal[,6:7],16, algorithm = "Lloyd")
centers<-as.data.frame(clusters$centers)
ggplot(data=Portugal, aes(Longitude, Latitude, colour = as.character(clusters$cluster)))+
  geom_point()+
  scale_colour_manual(breaks = clusters$cluster, 
                      values = unique(as.character(clusters$cluster)))+
  geom_point(data=centers, aes(Longitude, Latitude, color=unique(as.character(1:length(clusters$size)))), size=10, alpha=.3)+
  xlim(-10.5,-6.5)+
  ylim(37.5,41.5)
#Portugal$Group<-clusters$cluster

ggplot(data=subset(Portugal, Group == 2), aes(Longitude, Latitude))+
  geom_point()+
  xlim(-10.5,-6.5)+
  ylim(37.5,41.5)

#Portugal$Group

#Portugal<-transform(Portugal, Group = revalue(Group, c("9"="7")))

ggplot(data=Portugal, aes(Longitude, Latitude, colour = Group))+
  geom_point()+
  scale_color_manual(values = wes_palette(n = 15, name = "FantasticFox1", type = "continuous" ))
  #scale_colour_manual(breaks = c(1:15), 
  #                    values = 1:length(levels(Portugal$Group)),)+
  xlim(-10.5,-6.5)+
  ylim(37.5,41.5)

rows<-rownames(subset(Portugal, Group == 11 & Longitude > -9.3))

#Temp Code

ggplot(data=subset(Portugal, Group == "Porto"), aes(Longitude, Latitude))+
  geom_point()#+
  #xlim(-10.5,-6.5)+
  #ylim(37.5,41.5)

ggplot(data=subset(Portugal, Group == "Porto"), aes(Longitude, Latitude, color = as.character(clusters$cluster)))+
  geom_point()+
  scale_color_brewer(palette = "Set1")


#SubClusters
levels(Portugal$Group)

data=subset(Portugal, Group == "Lisbon")
ggplot(data=data, aes(Longitude, Latitude))+
  geom_point()

#clusters <- kmeans(data[,6:7],2, algorithm = "Hartigan-Wong")

ggplot(data=data, aes(Longitude, Latitude, color = as.character(clusters$cluster)))+
  geom_point()+
  scale_color_brewer(palette = "Paired")

rows<-as.numeric(rownames(data))

#Portugal$Location[rows]<-as.character(clusters$cluster)

#Portugal$Location <- Portugal$Location %>%
  #str_c() %>%
  str_replace_all(c(#"10" = "Arrabida Coast",
                    #"11" = "Baleeira",
                    #"12" = "Outlier",
                    "1" = "South", 
                    "2" = "North")) 
                    #"3" = "Pereiro"))
                    #"4" = "Montemor o Velho"))
                    #"5" = "Arrabida Inland",
                    #"6" = "Palmela",
                    #"7" = "Arrabida Coast",
                    #"8" = "Arrabida Inland",
                    #"9" = "Arrabida Farm"))
  
ggplot(data=subset(Portugal, Species == "robur" | Species == "rotundifolia" | Species == "suber" | Species == "faginea" | Species == "coccifera" | Species == "lusitanica"), aes(Longitude, Latitude, colour = Species))+
  geom_point()+
  scale_color_brewer(palette = "Set1")
  xlim(-10.5,-6.5)+
  ylim(37.5,41.5)

