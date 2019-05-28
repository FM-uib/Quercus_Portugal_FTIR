plsr_env <- function(data, form = "cbind(prec, temp, srad, elevation, soil_m_0.07, Latitude, Longitude) ~ FTIR.SG2", npc = 50, sel = c("prec", "temp", "srad", "elevation", "soil_m_0.07", "Latitude", "Longitude", "Group")){
  require(pls)
  pls_loo = plsr(reformulate(form), npc, data = data, scale = T, validation = "LOO")
  plot_data = as.data.frame(pls_loo[,])
  colnames(plot_data) = sapply(c(1:npc), function(x) paste0("PC",x))
  plot_data = cbind(plot_data, data[,sel])
  return(list("plsr" = pls_loo, "plot_data" = plot_data))
}

plsr_env_plot <- function(data, data_sub, pls, x, y, lds){
  require(ggplot2)
  require(stringr)
  require(grid)
  require(gridExtra)
  
  suber.prec.gg <- ggplot(data, aes_string(x, y)) +
    geom_point(size = 3, alpha = .1, show.legend = F) + 
    coord_equal(ratio = 1, xlim = c(min(data[,x])-1,max(data[,x])+1), 
                ylim = c(min(data[,y])+1, max(data[,y])+1)) +
    geom_point(data = data_sub, inherit.aes = F,  
               aes_string(x, y, shape = "Group", color = "prec", size = 5)) +
    scale_shape_manual(values = c(15,17,19,18))+
    scale_colour_gradient2(low = "#AFDFFF", mid = "#50B0FF",high = "#3347FF" , 
                           midpoint = 25, limits = c(0,50), name = "Precipitation")+
    guides(size = F, shape = F) + theme_bw() + 
    theme(text = element_text(size = 18), legend.position = "left",
          axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    xlab(paste0(x," (",round(explvar(pls)[as.numeric(str_sub(x, -1))],1)," %)")) + 
    ylab(paste0(y," (",round(explvar(pls)[as.numeric(str_sub(y, -1))],1)," %)"))
  
  suber.lalo.gg <- ggplot(data, aes_string(x, y, color = "Latitude", alpha = "Longitude")) +
    geom_point(size = 6) + 
    coord_equal(ratio = 1, xlim = c(min(data[,x])-1,max(data[,x])+1), 
                ylim = c(min(data[,y])+1, max(data[,y])+1)) +
    theme_bw() + theme(text = element_text(size = 18),
                       axis.title.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.title.y=element_blank(),
                       axis.text.y=element_blank()) +
    xlab(paste0(x," (",round(explvar(pls)[as.numeric(str_sub(x, -1))],1)," %)")) + 
    ylab(paste0(y," (",round(explvar(pls)[as.numeric(str_sub(y, -1))],1)," %)")) +
    scale_color_gradient2(low = "red", mid = "green", high = "dark blue", 
                          midpoint = mean(data$Latitude), name = "Latitude") +
    scale_alpha_continuous(range = c(1,.1), breaks = c(-7,-8,-9), name = "Longitude")
  
  suber.temp.gg <-ggplot(data, aes_string(x, y)) +
    geom_point(size = 3, alpha = .1, show.legend = F) + 
    coord_equal(ratio = 1, xlim = c(min(data[,x])-1,max(data[,x])+1), 
                ylim = c(min(data[,y])+1, max(data[,y])+1)) +
    theme_bw() + theme(text = element_text(size = 18), legend.position = "left") +
    xlab(paste0(x," (",round(explvar(pls)[as.numeric(str_sub(x, -1))],1)," %)")) + 
    ylab(paste0(y," (",round(explvar(pls)[as.numeric(str_sub(y, -1))],1)," %)")) +
    geom_point(data = data_sub, inherit.aes = F,  
               aes_string(x, y, shape = "Group", color = "temp", size = 5)) +
    scale_shape_manual(values = c(15,17,19,18))+
    scale_colour_gradient2(low = "#7AFE47", mid = "#FEE547",high = "#FF2903" , 
                           midpoint = 16, limits = c(12,20), name = "Temperature")+
    guides(size = F, shape = F)
  
  suber.soil.gg <- ggplot(data, aes_string(x, y)) +
    geom_point(size = 3, alpha = .1, show.legend = F) + 
    coord_equal(ratio = 1, xlim = c(min(data[,x])-1,max(data[,x])+1), 
                ylim = c(min(data[,y])+1, max(data[,y])+1)) +
    theme_bw() + theme(text = element_text(size = 18),
                       axis.title.y=element_blank(),
                       axis.text.y=element_blank()) +
    xlab(paste0(x," (",round(explvar(pls)[as.numeric(str_sub(x, -1))],1)," %)")) + 
    ylab(paste0(y," (",round(explvar(pls)[as.numeric(str_sub(y, -1))],1)," %)")) +
    geom_point(data = data_sub, inherit.aes = F,  
               aes_string(x, y, shape = "Group", color = "soil_m_0.07", size = 5)) +
    scale_shape_manual(values = c(15,17,19,18))+
    scale_colour_gradient(low = "#FF9B75",high = "#70BAFA", 
                          limits = c(.2,.4), name = "Soil Moisture")+
    guides(size = F)
  grobz <- lapply(list(suber.prec.gg, suber.lalo.gg, suber.temp.gg, suber.soil.gg), ggplotGrob)
  grobz.plot <- arrangeGrob( grobs = list(rbind(grobz[[1]], grobz[[3]], size = "last"),
                                          rbind(grobz[[2]], grobz[[4]], size = "last")),
                             ncol = 2)
  return(grobz.plot)}


  
  

  grid.draw(grobz.plot)
  

suber.pls.loo = plsr(cbind(prec, temp, srad, elevation, soil_m_0.07, Latitude, Longitude) ~ FTIR.SG2, npc, data = suber, scale = T, validation = "LOO")

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

ggplot(suber.plot.data, aes(PC1, PC2, color = Latitude, alpha = Longitude)) +
  geom_point(size = 6) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18)) +
  xlab(paste0("PC 1 (",round(explvar(suber.pls.loo)[1],1)," %)")) + ylab(paste0("PC 2 (",round(explvar(suber.pls.loo)[2],1)," %)"))+
  scale_color_gradient2(low = "red", mid = "green", high = "dark blue", midpoint = mean(suber.plot.data$Latitude), name = "Latitude") +
  scale_alpha_continuous(range = c(1,.1), breaks = c(-7,-8,-9), name = "Longitude")
#+geom_point(data = suber.plot.data_sub, inherit.aes = F,  aes(PC1, PC2, shape = Group, size = 5))

suber.pls.loo = plsr(cbind(prec, temp, srad, elevation, soil_m_0.07, Latitude, Longitude) ~ FTIR.SG2, npc, data = suber, scale = T, validation = "LOO")

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

ggplot(suber.plot.data, aes(PC1, PC2, color = Latitude, alpha = Longitude)) +
  geom_point(size = 6) + 
  coord_equal(ratio = 1, xlim = c(-15,17), ylim = c(-15,17)) +
  theme_bw() + theme(text = element_text(size = 18)) +
  xlab(paste0("PC 1 (",round(explvar(suber.pls.loo)[1],1)," %)")) + ylab(paste0("PC 2 (",round(explvar(suber.pls.loo)[2],1)," %)"))+
  scale_color_gradient2(low = "red", mid = "green", high = "dark blue", midpoint = mean(suber.plot.data$Latitude), name = "Latitude") +
  scale_alpha_continuous(range = c(1,.1), breaks = c(-7,-8,-9), name = "Longitude")