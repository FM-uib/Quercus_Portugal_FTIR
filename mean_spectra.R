library(here)
library(pls)
library(MASS)
library(caret)
library(directlabels)

mean_spectra <- function(data, ID){
  tmp = as.matrix(do.call(rbind,lapply(unique(ID), function(x) colMeans(data[ID == x,]))))
  return(tmp)
}

evaluate_pls <- function(npc, trained, data, train = TRUE){

  error <- rep(0, npc)
  con_M <- list()
  if (train){
    tested <- predict(trained, newdata = data[!data$train,], type = "score")
    for (i in 1:npc) {
      fitdata <- data.frame(Species = data$Sub_Spec[data$train], 
                            FTIR.score = I(trained$scores[,1:i, drop = FALSE]))
      testdata <- data.frame(Species = data$Sub_Spec[!data$train],
                             FTIR.score = I(tested[,1:i,drop=FALSE]))
      error[i] <- (nrow(tested) - sum(predict(lda(Species ~ FTIR.score, data = fitdata),
                                              newdata = testdata)$class == testdata$Species))/nrow(tested)
      con_M[[i]] <- I(confusionMatrix(predict(lda(Species ~ FTIR.score, data = fitdata), 
                                              newdata = testdata)$class, testdata$Species))
    }
    result <- list("error" = error, "conf_matrix" = con_M, "fitdata" = fitdata, "testdata" = testdata)
    return(result)
  }else{
    for (i in 1:npc) {
      fitdata <- data.frame(Species = data$Sub_Spec, 
                            FTIR.score = I(trained$scores[,1:i, drop = FALSE]))}
    result <- list("fitdata" = fitdata)
    return(result)
  }
    
}

pc_plots <- function(plot, loadings, pls, alpha = 1, size = 3) {
  require(ggplot2)
  theme_set(theme_bw())
  
  plot1 <- ggplot(plot, aes(PC1,PC2, color = Species)) +
    geom_hline(yintercept = 0, alpha = .5) + geom_vline(xintercept = 0, alpha = .5) +
    geom_point(size = size, alpha = alpha, aes(shape = Section)) + coord_equal() +
    scale_color_discrete() + stat_ellipse() +
    xlab(paste0("PC 1 (",round(explvar(pls)[1],1), " %)")) + ylab(paste0("PC 2 (",round(explvar(pls)[2],1), " %)")) + 
    scale_x_continuous(limits=c(-27, 15)) + scale_y_continuous(limits=c(-15, 22)) +
    scale_shape_manual(values = c(15:17)) +
    theme(legend.position = "none", text = element_text(size = 18))
  
  lds1 =subset(loadings, Comp == "L" | Comp == "S")
  plot1_ldg <- plot1 + geom_segment(data = lds1, inherit.aes = F,
                                    aes(x=0, xend=PC1*100, y=0, yend=PC2*100),
                                    color = "red", arrow=arrow(length=unit(0.05,"npc"))) +
    geom_text(data=lds1, inherit.aes = F,
              aes(x=PC1*100,y=PC2*100,label=Comp,
                                 hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), color = "black", size=6)
  
  plot2 <- ggplot(plot,aes(PC3,PC4, color = Species)) +
    geom_hline(yintercept = 0, alpha = .5) + geom_vline(xintercept = 0, alpha = .5) +
    geom_point(size = size, alpha = alpha, aes(shape = Section)) + coord_equal() +
    scale_color_discrete() + stat_ellipse() +
    xlab(paste0("PC 3 (", round(explvar(pls)[3],1), " %)")) + ylab(paste0("PC 4 (", round(explvar(pls)[4],1), " %)")) +
    scale_x_continuous(limits=c(-27, 15)) + scale_y_continuous(limits=c(-15, 22)) +
    scale_shape_manual(values = c(15:17)) +
    theme(text = element_text(size = 18))
  
  lds2 =subset(loadings, Comp == "C" | Comp == "P")
  plot2_ldg <- plot2 + geom_segment(data=lds2, inherit.aes = F,
                                    aes(x=0, xend=PC3*100, y=0, yend=PC4*100),
                                    color = "red", arrow=arrow(length=unit(0.05,"npc"))) +
    geom_text(data=lds2, inherit.aes = F,
              aes(x=PC3*100,y=PC4*100,label=Comp,
                                 hjust=0.5*(1-sign(PC3)),vjust=0.5*(1-sign(PC4))), color = "black", size=6)
  results <- list(plot1, plot1_ldg, plot2, plot2_ldg)
  return(results)
}

ldgs.data <- function(pca){
  if(is.matrix(pca$rotation)) 
    ldngs <- pca$rotation
  else 
    ldngs <- pca$loadings
  ldngs <- ldngs[,1:10]
  ldngs <- as.data.frame(ldngs)
  ldngs$wavenumber <- as.numeric(rownames(ldngs))
  rownames(ldngs) <- round(as.numeric(rownames(ldngs)))
  ldngs.peaks <- ldngs[c("1745", "1462", "721",
                         "1651", "1641", "1551", "1535",
                         "1107", "1076", "1055", "1028", "995",
                         "1605", "1516", "1171", "852", "833", "816"),-615]
  ldngs.peaks$Comp<-c(rep("L",3), rep("P", 4), rep("C",5), rep("S",6))
  colnames(ldngs.peaks)[1:10] <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")
  ldngs.df <- melt(ldngs, id = "wavenumber")
  colnames(ldngs.df)<-c("Wavenumber", "PC", "SG2")
  return(list(ldngs.df,ldngs.peaks))
}

subset <- readRDS(file = here("Data","Input", "data_WC.rds"))

selection <- c("ID", "Section", "Genus", "Species", "Sub_Spec", "Latitude", "Longitude", "Group", "Location", "train", "elevation")

data <- subset[subset$MRep == "03", selection]

data$FTIR = I(mean_spectra(subset$FTIR, subset$ID))
data$FTIR.SG2 = I(mean_spectra(subset$FTIR.SG2, subset$ID))
data$Species.HO = I(mean_spectra(subset$Species.HO, subset$ID))
data$env_april_WS = I(mean_spectra(subset$env_april_WS, subset$ID))
data$env_april_WC = I(mean_spectra(subset$env_april_WC, subset$ID))

saveRDS(data, file = here("Data","Input", "data_meaned.rds"))
data = readRDS(file = here("Data", "Input", "data_meaned.rds"))
env = readRDS(file = here("Data", "Output", "env_WS_kriged.rds"))
soil_moist = readRDS(file = here("Data", "Output", "soil_moisture.rds"))

npc = 50

#Crossvalidated
pls <- cppls(Species.HO ~ FTIR.SG2, npc, data = data[data$train,], scale = T)
pls.cv <- crossval(pls, segments = 10)
pls.cv.eval <- evaluate_pls(npc, pls.cv, data, train = T)

pls.loo <- cppls(Species.HO ~ FTIR.SG2, npc, data = data[data$train,], scale = T, validation = "LOO")
pls.loo.eval <- evaluate_pls(npc, pls.loo, data, train = T)

PLS.plot.data<- data.frame(Species = factor(pls.loo.eval$fitdata$Species),
                           Section = data$Section[data$train],
                           Group = data$Group[data$train],
                           Location = data$Location[data$train],
                           PC1 = pls.loo.eval$fitdata$FTIR.score[,1],
                           PC2 = pls.loo.eval$fitdata$FTIR.score[,2],
                           PC3 = pls.loo.eval$fitdata$FTIR.score[,3],
                           PC4 = pls.loo.eval$fitdata$FTIR.score[,4],
                           PC5 = pls.loo.eval$fitdata$FTIR.score[,5])

plot_ly(x=PLS.plot.data$PC1,y=PLS.plot.data$PC2,z=PLS.plot.data$PC3,type = "scatter3d", mode = "markers", color = PLS.plot.data$Species)
plot_ly(x=PLS.plot.data$PC1,y=PLS.plot.data$PC2,z=PLS.plot.data$PC4,type = "scatter3d", mode = "markers", color = PLS.plot.data$Species)
plot_ly(x=PLS.plot.data$PC1,y=PLS.plot.data$PC3,z=PLS.plot.data$PC4,type = "scatter3d", mode = "markers", color = PLS.plot.data$Species)

pls.ldgs <- ldgs.data(pls.loo)[[2]]
pls.cv.plots <- pc_plots(PLS.plot.data, pls.ldgs, pls.loo)
pls.cv.plots.a <- pc_plots(PLS.plot.data, pls.ldgs, pls.loo, alpha = .5)



grid.newpage()
PCs <- grid.draw(cbind(ggplotGrob(pls.cv.plots[[1]]), ggplotGrob(pls.cv.plots[[3]]), size = "last"))

grid.newpage()
PC.lds <- grid.draw(cbind(ggplotGrob(pls.cv.plots.a[[2]]), ggplotGrob(pls.cv.plots.a[[4]]), size = "last"))

ggsave("PCs.png", plot = PCs, device = "png", path = here("R", "figures"), width = 35, height = 10, units = "cm", dpi = 600)

ggsave("PC_lds.png", plot = PC.lds, device = "png", path = here("R", "figures"), width = 35, height = 10, units = "cm", dpi = 600)



ggplot(PLS.plot.data,aes(PC1,PC2, color = Species)) +
  geom_point(size = 3, alpha = .1) + coord_equal() +
  scale_color_discrete() + stat_ellipse() +
  theme_bw() + theme(text = element_text(size = 18)) +
  geom_point(data = subset(PLS.plot.data, Species == "suber" & Group == "Arrabida" | Species == "suber" & Group == "Alijo"), aes(PC1, PC2, color = Group), size = 3) + stat_ellipse()

#Suber env
data <- cbind(data,dplyr::full_join(env14,soil_moist)[,-1])

suber = subset(data, Species == "suber")
suber.pls.loo = cppls(cbind(prec, temp, srad, elevation, soil_m_0.07, Latitude, Longitude) ~ FTIR.SG2, npc, data = suber, scale = T, validation = "LOO")
#suber.pls.cv = crossval(suber.pls, segments = 5)
suber.pls.eval = evaluate_pls(npc, suber.pls.loo, suber, train = F)

suber.plot.data<- data.frame(Species = factor(suber.pls.eval$fitdata$Species),
                           PC1 = suber.pls.eval$fitdata$FTIR.score[,1],
                           PC2 = suber.pls.eval$fitdata$FTIR.score[,2],
                           PC3 = suber.pls.eval$fitdata$FTIR.score[,3],
                           PC4 = suber.pls.eval$fitdata$FTIR.score[,4],
                           PC5 = suber.pls.eval$fitdata$FTIR.score[,5])
suber.plot.data <- cbind(suber[,c(6,7,8,9,11,18:24)], suber.plot.data)
suber.plot.data_sub <- subset(suber.plot.data, Group == "Arrabida" | Group == "Alijo" | Group == "Porto"| Group == "Coimbra")
suber.plot.data_sub$Group<-factor(suber.plot.data_sub$Group)

suber.prec.gg <- ggplot(suber.plot.data, aes(PC3, PC1)) +
  geom_point(size = 3, alpha = .1, show.legend = F) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18), legend.position = "left",
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank()) +
  xlab(paste0("PC 3 (",round(explvar(suber.pls.loo)[3],1)," %)")) + ylab(paste0("PC 1 (",round(explvar(suber.pls.loo)[1],1)," %)")) +
  geom_point(data = suber.plot.data_sub, inherit.aes = F,  aes(PC3, PC1, shape = Group, color = prec, size = 5)) +
  scale_shape_manual(values = c(15,17,19,18))+
  scale_colour_gradient2(low = "#AFDFFF", mid = "#50B0FF",high = "#3347FF" , midpoint = 25, limits = c(0,50), name = "Precipitation")+
  guides(size = F, shape = F)

suber.lalo.gg <- ggplot(suber.plot.data, aes(PC3, PC1, color = Latitude, alpha = Longitude)) +
  geom_point(size = 6) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank()) +
  xlab(paste0("PC 3 (",round(explvar(suber.pls.loo)[3],1)," %)")) + ylab(paste0("PC 1 (",round(explvar(suber.pls.loo)[1],1)," %)"))+
  scale_color_gradient2(low = "red", mid = "green", high = "dark blue", midpoint = mean(suber.plot.data$Latitude), name = "Latitude") +
  scale_alpha_continuous(range = c(1,.1), breaks = c(-7,-8,-9), name = "Longitude")

suber.temp.gg <-ggplot(suber.plot.data, aes(PC3, PC1)) +
  geom_point(size = 3, alpha = .1, show.legend = F) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18), legend.position = "left") +
  xlab(paste0("PC 3 (",round(explvar(suber.pls.loo)[3],1)," %)")) + ylab(paste0("PC 1 (",round(explvar(suber.pls.loo)[1],1)," %)")) +
  geom_point(data = suber.plot.data_sub, inherit.aes = F,  aes(PC3, PC1, shape = Group, color = temp, size = 5)) +
  scale_shape_manual(values = c(15,17,19,18))+
  scale_colour_gradient2(low = "#7AFE47", mid = "#FEE547",high = "#FF2903" , midpoint = 16, limits = c(12,20), name = "Temperature")+
  guides(size = F, shape = F)

suber.soil.gg <- ggplot(suber.plot.data, aes(PC3, PC1)) +
  geom_point(size = 3, alpha = .1, show.legend = F) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank()) +
  xlab(paste0("PC 3 (",round(explvar(suber.pls.loo)[3],1)," %)")) + ylab(paste0("PC 1 (",round(explvar(suber.pls.loo)[1],1)," %)")) +
  geom_point(data = suber.plot.data_sub, inherit.aes = F,  aes(PC3, PC1, shape = Group, color = soil_m_0.07, size = 5)) +
  scale_shape_manual(values = c(15,17,19,18))+
  scale_colour_gradient(low = "#FF9B75",high = "#70BAFA", limits = c(.2,.4), name = "Soil Moisture")+
  guides(size = F)


grobz <- lapply(list(suber.prec.gg, suber.lalo.gg, suber.temp.gg, suber.soil.gg), ggplotGrob)
grobz.plot <- arrangeGrob( grobs = list(rbind(grobz[[1]], grobz[[3]], size = "last"),
                                        rbind(grobz[[2]], grobz[[4]], size = "last")),
                           ncol = 2)
grid.draw(grobz.plot)

ggsave("env_plots_suber.png", plot = grobz.plot, device = "png", path = here("R", "figures"), width = 35, height = 20, units = "cm", dpi = 600)


suber.prec.gg <- ggplot(suber.plot.data, aes(PC2, PC4)) +
  geom_point(size = 3, alpha = .1, show.legend = F) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18), legend.position = "left",
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank()) +
  xlab(paste0("PC 2 (",round(explvar(suber.pls.loo)[2],1)," %)")) + ylab(paste0("PC 4 (",round(explvar(suber.pls.loo)[4],1)," %)")) +
  geom_point(data = suber.plot.data_sub, inherit.aes = F,  aes(PC2, PC4, shape = Group, color = prec, size = 5)) +
  scale_shape_manual(values = c(15,17,19,18))+
  scale_colour_gradient2(low = "#AFDFFF", mid = "#50B0FF",high = "#3347FF" , midpoint = 25, limits = c(0,50), name = "Precipitation")+
  guides(size = F, shape = F)

suber.lalo.gg <- ggplot(suber.plot.data, aes(PC2, PC4, color = Latitude, alpha = Longitude)) +
  geom_point(size = 6) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank()) +
  xlab(paste0("PC 2 (",round(explvar(suber.pls.loo)[2],1)," %)")) + ylab(paste0("PC 4 (",round(explvar(suber.pls.loo)[4],1)," %)"))+
  scale_color_gradient2(low = "red", mid = "green", high = "dark blue", midpoint = mean(suber.plot.data$Latitude), name = "Latitude") +
  scale_alpha_continuous(range = c(1,.1), breaks = c(-7,-8,-9), name = "Longitude")

suber.temp.gg <-ggplot(suber.plot.data, aes(PC2, PC4)) +
  geom_point(size = 3, alpha = .1, show.legend = F) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18), legend.position = "left") +
  xlab(paste0("PC 2 (",round(explvar(suber.pls.loo)[2],1)," %)")) + ylab(paste0("PC 4 (",round(explvar(suber.pls.loo)[4],1)," %)")) +
  geom_point(data = suber.plot.data_sub, inherit.aes = F,  aes(PC2, PC4, shape = Group, color = temp, size = 5)) +
  scale_shape_manual(values = c(15,17,19,18))+
  scale_colour_gradient2(low = "#7AFE47", mid = "#FEE547",high = "#FF2903" , midpoint = 16, limits = c(12,20), name = "Temperature")+
  guides(size = F, shape = F)

suber.soil.gg <- ggplot(suber.plot.data, aes(PC2, PC4)) +
  geom_point(size = 3, alpha = .1, show.legend = F) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank()) +
  xlab(paste0("PC 2 (",round(explvar(suber.pls.loo)[2],1)," %)")) + ylab(paste0("PC 4 (",round(explvar(suber.pls.loo)[4],1)," %)")) +
  geom_point(data = suber.plot.data_sub, inherit.aes = F,  aes(PC2, PC4, shape = Group, color = soil_m_0.07, size = 5)) +
  scale_shape_manual(values = c(15,17,19,18))+
  scale_colour_gradient(low = "#FF9B75",high = "#70BAFA", limits = c(.2,.4), name = "Soil Moisture")+
  guides(size = F)


grobz <- lapply(list(suber.prec.gg, suber.lalo.gg, suber.temp.gg, suber.soil.gg), ggplotGrob)
grobz.plot <- arrangeGrob( grobs = list(rbind(grobz[[1]], grobz[[3]], size = "last"),
                                        rbind(grobz[[2]], grobz[[4]], size = "last")),
                           ncol = 2)
grid.draw(grobz.plot)

ggsave("env_plots_suber_30.png", plot = grobz.plot, device = "png", path = here("R", "figures"), width = 35, height = 20, units = "cm", dpi = 600)

ord.surf <- function(x, y){
  require(vegan)
  ord.surf <- ordisurf(x = x, y = y, plot = FALSE)
  grid <- ord.surf$grid
  ordi <- expand.grid(x = grid$x, y = grid$y)
  ordi$z <- as.vector(grid$z)
  ordi <- data.frame(na.omit(ordi))
  results <- list(ordi, ord.surf)
  return(results)
}

pls.scores <- suber.pls.loo$scores[,c(3,1)]

sub.surf.prec <- ord.surf(x = pls.scores, y = suber$prec)
sub.surf.srad <- ord.surf(x = pls.scores, y = suber$srad)
sub.surf.temp <- ord.surf(x = pls.scores, y = suber$temp)
sub.surf.elev <- ord.surf(x = pls.scores, y = suber$elevation)
sub.surf.lat <- ord.surf(x = pls.scores, y = suber$Latitude)
sub.surf.lon <- ord.surf(x = pls.scores, y = suber$Longitude)
sub.surf.soil <- ord.surf(x = pls.scores, y = suber$soil_m_0.07)

plot <- ggplot(suber.plot.data, aes(PC3,PC1, color = Species)) +
  geom_point(size = 3) +
  scale_color_discrete() +
  xlab(paste0("PC 3 (",round(explvar(suber.pls.loo)[3],1)," %)")) + ylab(paste0("PC 1 (",round(explvar(suber.pls.loo)[1],1)," %)")) +
  theme_bw() + theme(text = element_text(size = 18)) +
  coord_equal(ratio = 1)

temp.map <- plot+stat_contour(data = sub.surf.temp[[1]], 
                              aes(x = x, y = y, z = z),
                              binwidth = .5, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = sub.surf.temp[[1]], 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = .5) + ggtitle("Temperature")

prec.map <- plot+stat_contour(data = sub.surf.prec[[1]], 
                              aes(x = x, y = y, z = z),
                              binwidth = 10, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = sub.surf.prec[[1]], 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = 10) + ggtitle("Precipitation")

elev.map <- plot+stat_contour(data = sub.surf.elev[[1]], 
                              aes(x = x, y = y, z = z),
                              binwidth = 50, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = sub.surf.elev[[1]], 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = 50) + ggtitle("Elevation")

srad.map <- plot+stat_contour(data = sub.surf.srad[[1]], 
                              aes(x = x, y = y, z = z),
                              binwidth = 100, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = sub.surf.srad[[1]], 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = 100) + ggtitle("Solar Radiation")

lat.map <- plot+stat_contour(data = sub.surf.lat[[1]], 
                              aes(x = x, y = y, z = z),
                              binwidth = .25, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = sub.surf.lat[[1]], 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = .25) + ggtitle("Latitude")

lon.map <- plot + stat_contour(data = sub.surf.lon[[1]], 
                              aes(x = x, y = y, z = z),
                              binwidth = .25, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = sub.surf.lon[[1]], 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = .25) + ggtitle("Longitude")

soil.map <- plot + stat_contour(data = sub.surf.soil[[1]], 
                               aes(x = x, y = y, z = z),
                               binwidth = .05, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = sub.surf.soil[[1]], 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = .25) + ggtitle("Soil Moisture")

plot.group <- plot + geom_point(data = subset(suber.plot.data, Species == "suber" & Group == "Arrabida" | Species == "suber" & Group == "Alijo" | Species == "suber" & Group == "Porto" | Species == "suber" & Group == "Coimbra"), aes(PC3, PC2, color = Group), size = 3) + stat_ellipse()
ggsave("suber_group.png", plot = plot.group, device = "png", path = here("R", "figures", "ord_plot_WS"), width = 20, height = 12, units = "cm", dpi = 600)

ggsave("suber_temp_map_WS.png", plot = temp.map, device = "png", path = here("R", "figures", "ord_plot_WS"), width = 20, height = 12, units = "cm", dpi = 600)

ggsave("suber_prec_map_WS.png", plot = prec.map, device = "png", path = here("R", "figures", "ord_plot_WS"), width = 20, height = 12, units = "cm", dpi = 600)

ggsave("suber_elev_map_WS.png", plot = elev.map, device = "png", path = here("R", "figures", "ord_plot_WS"), width = 20, height = 12, units = "cm", dpi = 600)

ggsave("suber_srad_map_WS.png", plot = srad.map, device = "png", path = here("R", "figures", "ord_plot_WS"), width = 20, height = 12, units = "cm", dpi = 600)

ggsave("suber_lat_map_WS.png", plot = lat.map, device = "png", path = here("R", "figures", "ord_plot_WS"), width = 20, height = 12, units = "cm", dpi = 600)

ggsave("suber_lon_map_WS.png", plot = lon.map, device = "png", path = here("R", "figures", "ord_plot_WS"), width = 20, height = 12, units = "cm", dpi = 600)

SumTable <- data %>%
  group_by(Group, Sub_Spec) %>%
  summarize(n = n(), Lat = mean(Latitude), Lon = mean(Longitude))
SumTableOrd <- arrange(SumTable, Lat, Lon, decreasing = TRUE)
