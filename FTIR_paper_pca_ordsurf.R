setwd("O:/PhD/Data/Portugal 2018/paper")
# PCA with full set
library(vegan)
library(ggplot2)

load(file = "data.rda")

pca <- prcomp(subset$FTIR.SG2,
              center = T,
              scale = T)

env.vec <- envfit(pca, pca.data[,20:22])

save(pca, env.vec, file = "pca_ord_surf.rda")

pca.biplot <- biplot(pca, choices = c(1,2), groups = pca.data$Sub_Spec, alpha = 0.1)

ldgs.data <- function(pca){
  if(is.matrix(pca$rotation)) 
    ldngs <- pca$rotation
  else 
    ldngs <- pca$loadings
  ldngs <- ldngs[,1:10]
  ldngs <- as.data.frame(ldngs)
  ldngs$wavenumber <- as.numeric(rownames(ldngs))
  rownames(ldngs) <- round(as.numeric(rownames(ldngs)))
  ldngs.peaks <- ldngs[c("1605", "1516", "1171", "833", "1745","1462","721","1107", "1055", "1028","1076", "995", "1651","1551"),-615]
  ldngs.peaks$Comp<-c(rep("S",4), rep("L", 3), rep("C",5), rep("P",2))
  ldngs.df <- melt(ldngs, id = "wavenumber")
  colnames(ldngs.df)<-c("Wavenumber", "PC", "SG2")
  return(list(ldngs.df,ldngs.peaks))
}

ldngs.plot <- function(ldngs, comp = "PC1"){
  g <- ggplot(data = subset(ldngs, PC == comp), aes(Wavenumber, SG2))+
    geom_line(alpha=0.5)+theme_bw()+
    theme(text = element_text(size = 18), axis.title.x = element_blank())+
    scale_x_reverse(limits = c(1800,700), breaks = seq(1800,700,-100))+ylab(comp)+
    geom_hline(yintercept = 0, linetype="dashed")+
    sporopollenin+lipids+carbs+proteins
  return(g)
}

loading <- ldgs.data(pca)

p1 <- ldngs.plot(loading[[1]], comp = "PC1")
p2 <- ldngs.plot(loading[[1]], comp = "PC2")
p3 <- ldngs.plot(loading[[1]], comp = "PC3")
p4 <- ldngs.plot(loading[[1]], comp = "PC4")

grid.arrange(p1, p2, p3, p4, nrow = 4)

ldngs.peaks <- loading[[2]]

pca.biplot.lp <- pca.biplot +
  geom_segment(data=ldngs.peaks, aes(x=0, xend=PC1*100, y=0, yend=PC2*100), #color = "red"
               color = "red", arrow=arrow(length=unit(0.05,"npc"))) +
  geom_text(data=ldngs.peaks, 
            aes(x=PC1*100,y=PC2*100,label=ldngs.peaks$Comp,
                hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), 
            color = "black", size=6) #color="red"
pca.biplot.lp

env.vectors<-as.data.frame(env.vec$vectors$arrows)
rownames(env.vectors) <- c("Temperature", "Elevation", "Precipitation")

biplot.ev <- function(biplot, vectors){
  vectors$names <- rownames(vectors)
  pca.biplot.ev <- biplot +
    geom_segment(data=vectors, aes(x=0, xend=PC1*10, y=0, yend=PC2*10), 
                 color = "blue", arrow=arrow(length=unit(0.05,"npc"))) +
    geom_text(data=vectors, 
              aes(x=PC1*10,y=PC2*10,label=names,
                  hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), 
              color = "black", size=6, check_overlap = F)
  return(pca.biplot.ev)
}
ev.all <- biplot.ev(biplot = pca.biplot, vectors = env.vectors)
ev.temp <- biplot.ev(biplot = pca.biplot, vectors = env.vectors[1,])
ev.ele <- biplot.ev(biplot = pca.biplot, vectors = env.vectors[2,])
ev.precip <- biplot.ev(biplot = pca.biplot, vectors = env.vectors[3,])

# Ordisurf
ord.surf <- function(x, y){
  ord.surf <- ordisurf(x = x, y = y, plot = FALSE)
  grid <- ord.surf$grid
  ordi <- expand.grid(x = grid$x, y = grid$y)
  ordi$z <- as.vector(grid$z)
  ordi <- data.frame(na.omit(ordi))
  return(ordi)
}

ord.surf.temp <- ord.surf(x = pca, y = pca.data[,20])
ord.surf.ele <- ord.surf(x = pca, y = pca.data[,21])
ord.surf.precip <- ord.surf(x = pca, y = pca.data[,22])

temp.map <- ev.temp+stat_contour(data = ord.surf.temp, 
                        aes(x = x, y = y, z = z),
                        binwidth = .2, size = 1, linetype = "dashed", colour = "grey50")+
  geom_point(data = ord.surf.temp, aes(x,y))
ele.map <- ev.ele+stat_contour(data = ord.surf.ele, 
                     aes(x = x, y = y, z = z),
                     binwidth = 20, size = 1, linetype = "dashed", colour = "grey50")
precip.map <- ev.precip+stat_contour(data = ord.surf.precip, 
                     aes(x = x, y = y, z = z),
                     binwidth = 5, size = 1, linetype = "dashed", colour = "grey50")
