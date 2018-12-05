setwd("O:/PhD/Data/Portugal 2018/paper")
library(vegan)
library(ggplot2)

load(file = "data.rda")
load(file = "pls.rda")
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
  ord.surf <- ordisurf(x = x, y = y, plot = FALSE)
  grid <- ord.surf$grid
  ordi <- expand.grid(x = grid$x, y = grid$y)
  ordi$z <- as.vector(grid$z)
  ordi <- data.frame(na.omit(ordi))
  return(ordi)
}

pls.scores <- scores(pls.train.yadd, choices = c(1,2))

ord.surf.pls.train.prec <- ord.surf(x = pls.scores, y = subset$mean_precip[subset$train])
ord.surf.pls.train.temp <- ord.surf(x = pls.scores, y = subset$mean_temp[subset$train])
ord.surf.pls.train.elev <- ord.surf(x = pls.scores, y = subset$elevation[subset$train])

pls.yadd.plot.data<- data.frame(Species = factor(fitdata2$Species),
                           PC1 = fitdata2$FTIR.score[,1],
                           PC2 = fitdata2$FTIR.score[,2],
                           PC3 = fitdata2$FTIR.score[,3],
                           PC4 = fitdata2$FTIR.score[,4],
                           PC5 = fitdata2$FTIR.score[,5])
plot <- ggplot(pls.yadd.plot.data,aes(PC1,PC2, color = Species))+
  geom_point(alpha = 0.1)+
  scale_color_discrete()+stat_ellipse()+
  theme_bw()+theme(text = element_text(size = 18))+
  coord_equal(ratio = 1)

temp.map <- plot+stat_contour(data = ord.surf.pls.train.temp, 
                                 aes(x = x, y = y, z = z),
                                 binwidth = .2, size = 1, linetype = "dashed", colour = "grey50")
prec.map <- plot+stat_contour(data = ord.surf.pls.train.prec, 
                              aes(x = x, y = y, z = z),
                              binwidth = 5, size = 1, linetype = "dashed", colour = "grey50")
elev.map <- plot+stat_contour(data = ord.surf.pls.train.elev, 
                              aes(x = x, y = y, z = z),
                              binwidth = 20, size = 1, linetype = "dashed", colour = "grey50")
