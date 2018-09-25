setwd("O:/PhD/Data/Portugal 2018/paper")
library(caret)
library(MASS)

load(file = "data.rda")

train$FTIR.SG2 <- train$FTIR.SG2[,-c(1:5,620:624)]
test$FTIR.SG2 <- test$FTIR.SG2[,-c(1:5,620:624)]

pca.2d <- prcomp(train$FTIR.SG2,
       center = T,
       scale = T)

save(pca.2d, file = "pca_classification.rda")

eigs <- pca.2d$sdev^2


biplot <- function(pca, choices = c(1,2), groups = train$Sub_Spec, alpha = 1){
  eigs <- pca$sdev^2
  PCs<-rbind(
    SD = sqrt(eigs),
    Proportion = eigs/sum(eigs),
    Cumulative = cumsum(eigs)/sum(eigs))
  colnames(PCs) <- colnames(pca$x)
  
  dfr <- as.data.frame(pca$x)
  dfr$Species <- groups
  plot <- ggplot(dfr,aes(dfr[,choices[1]], dfr[,choices[2]], colour = Species))+
    geom_point(alpha = alpha)+stat_ellipse()+
    theme(aspect.ratio = 1, text = element_text(size = 30))+
    theme_bw()+geom_vline(xintercept = 0, linetype = "dashed")+ 
    geom_hline(yintercept = 0,linetype = "dashed")+
    xlab(paste(colnames(PCs)[choices[1]]," ",round(PCs[2,choices[1]]*100, 2),"%", sep = ""))+
    ylab(paste(colnames(PCs)[choices[2]]," ",round(PCs[2,choices[2]]*100, 2),"%", sep = ""))
  return(plot)
}

plot <- biplot(pca.2d)

ggsave("PC1_PC2.png", width = 15, height = 15, units = "cm", dpi = 320, plot = plot)

pred <- predict(pca.2d, newdata=test$FTIR.SG2)

fitdata.pca <- cbind(train$Sub_Spec, as.data.frame(pca.2d$x))
testdata.pca <- cbind(test$Sub_Spec, as.data.frame(pred))

pca.conv <- confusionMatrix(predict(lda(train$Sub_Spec ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = fitdata.pca),newdata = testdata.pca)$class, 
                testdata.pca[,1])
