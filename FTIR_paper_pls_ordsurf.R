setwd("O:/PhD/Data/Portugal 2018/paper")
library(here)
library(vegan)
library(ggplot2)

subset <- readRDS(file = here("Data", "Input", "data_WC.rds"))
load(file = here("Data", "Output", "pls.rda"))
# create scores for test and train at different components
for (i in 1:10){
  fitdata1 <- data.frame(Species = subset$Sub_Spec[subset$train],
                         FTIR.score = I(pls.train$scores[,1:i,drop=FALSE]))
  testdata1 <- data.frame(Species = subset$Sub_Spec[!subset$train],
                          FTIR.score = I(pls.test[,1:i,drop=FALSE]))
  
  fitdata2 <- data.frame(Species = subset$Sub_Spec[subset$train],
                         FTIR.score = I(pls.train.yadd$scores[,1:i,drop=FALSE]))
  testdata2 <- data.frame(Species = subset$Sub_Spec[!subset$train],
                          FTIR.score = I(pls.test.yadd[,1:i,drop=FALSE]))
  
  fitdata3 <- data.frame(Species = subset$Sub_Spec[subset$train],
                         FTIR.score = I(plsr.train$scores[,1:i,drop=FALSE]))
  testdata3 <- data.frame(Species = subset$Sub_Spec[!subset$train],
                          FTIR.score = I(plsr.test[,1:i,drop=FALSE]))
  
  conv.cpls<- confusionMatrix(predict(lda(Species ~ FTIR.score, data = fitdata1),
                                      newdata = testdata1)$class, testdata1$Species)
  conv.cpls.yadd<- confusionMatrix(predict(lda(Species ~ FTIR.score, data = fitdata2),newdata = testdata2)$class, 
                                   testdata2$Species)
  conv.plsr<-confusionMatrix(predict(lda(Species ~ FTIR.score, data = fitdata3),newdata = testdata3)$class, 
                             testdata3$Species)
}
load(file = "pca_ord_surf.rda")

ord.surf <- function(x, y){
  require(vegan)
  ord.surf <- ordisurf(x = x, y = y, plot = FALSE)
  grid <- ord.surf$grid
  ordi <- expand.grid(x = grid$x, y = grid$y)
  ordi$z <- as.vector(grid$z)
  ordi <- data.frame(na.omit(ordi))
  return(ordi)
}

pls.scores <- scores(pls.train, choices = c(1,2))

ord.surf.pls.train.prec <- ord.surf(x = pls.scores, y = subset$env_april_WC[subset$train,1])
ord.surf.pls.train.srad <- ord.surf(x = pls.scores, y = subset$env_april_WC[subset$train,3])
ord.surf.pls.train.temp <- ord.surf(x = pls.scores, y = subset$env_april_WC[subset$train,2])
ord.surf.pls.train.elev <- ord.surf(x = pls.scores, y = subset$elevation[subset$train])

pls.plot.data<- data.frame(Species = factor(pls.eval$fitdata$Species),
                           PC1 = pls.eval$fitdata$FTIR.score[,1],
                           PC2 = pls.eval$fitdata$FTIR.score[,2],
                           PC3 = pls.eval$fitdata$FTIR.score[,3],
                           PC4 = pls.eval$fitdata$FTIR.score[,4],
                           PC5 = pls.eval$fitdata$FTIR.score[,5])

plot <- ggplot(pls.plot.data,aes(PC1,PC2, color = Species))+
  geom_point(alpha = 0.1)+
  scale_color_discrete()+stat_ellipse()+
  theme_bw()+theme(text = element_text(size = 18))+
  coord_equal(ratio = 1)

temp.map <- plot+stat_contour(data = ord.surf.pls.train.temp, 
                                 aes(x = x, y = y, z = z),
                                 binwidth = .5, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = ord.surf.pls.train.temp, 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = .5)
prec.map <- plot+stat_contour(data = ord.surf.pls.train.prec, 
                              aes(x = x, y = y, z = z),
                              binwidth = 10, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = ord.surf.pls.train.prec, 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = 10)
elev.map <- plot+stat_contour(data = ord.surf.pls.train.elev, 
                              aes(x = x, y = y, z = z),
                              binwidth = 50, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = ord.surf.pls.train.elev, 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = 50)

srad.map <- plot+stat_contour(data = ord.surf.pls.train.srad, 
                              aes(x = x, y = y, z = z),
                              binwidth = 100, size = 1, linetype = "dashed", colour = "grey50") +
  geom_dl(aes(x = x, y = y, z = z, label = ..level..), 
          data = ord.surf.pls.train.srad, 
          method = list("bottom.pieces", cex = .6, vjust = -.3), 
          stat = "contour", inherit.aes = FALSE, binwidth = 100)

ggsave("temp_map_WC.png", plot = temp.map, device = "png", path = here("R", "figures", "ord_plot_WC"), width = 20, height = 12, units = "cm", dpi = 600)

ggsave("prec_map_WC.png", plot = prec.map, device = "png", path = here("R", "figures", "ord_plot_WC"), width = 20, height = 12, units = "cm", dpi = 600)

ggsave("elev_map_WC.png", plot = elev.map, device = "png", path = here("R", "figures", "ord_plot_WC"), width = 20, height = 12, units = "cm", dpi = 600)

ggsave("srad_map_WC.png", plot = srad.map, device = "png", path = here("R", "figures", "ord_plot_WC"), width = 20, height = 12, units = "cm", dpi = 600)
