setwd("O:/PhD/Data/Portugal 2018/paper")
library(randomForest)
library(gridExtra)
library(ggplot2)

load(file = "data.rda")

test$Sub_Spec <- factor(test$Sub_Spec)
train$Sub_Spec <- factor(train$Sub_Spec)

rf <- randomForest(x = train$FTIR.SG2, 
                   y = train$Sub_Spec,
                   xtest = test$FTIR.SG2,
                   ytest = test$Sub_Spec,
                   ntree = 1000, maxnodes = 25,
                   importance = TRUE, 
                   proximity = TRUE)

save(rf, file = "randomforest.rda")
rf.importance <- as.data.frame(rf$importance)
rf.importance$wavenumbers <- as.numeric(rownames(rf.importance))


p1 <- ggplot(data = rf.importance, aes(wavenumbers, broteroi))+
  geom_line()+ theme_bw()+ theme(axis.title.x = element_blank())+
  scale_x_continuous(limits = c(700,1800), breaks = seq(700,1800,100))+
  carbs + proteins + lipids + sporopollenin

p2 <- ggplot(data = rf.importance, aes(wavenumbers, robur))+
  geom_line()+ theme_bw()+ theme(axis.title.x = element_blank())+
  scale_x_continuous(limits = c(700,1800), breaks = seq(700,1800,100))+
  carbs + proteins + lipids + sporopollenin

p3 <- ggplot(data = rf.importance, aes(wavenumbers, estremadurensis))+
  geom_line()+ theme_bw()+ theme(axis.title.x = element_blank())+
  scale_x_continuous(limits = c(700,1800), breaks = seq(700,1800,100))+
  carbs + proteins + lipids + sporopollenin

p4 <- ggplot(data = rf.importance, aes(wavenumbers, coccifera))+
  geom_line()+ theme_bw()+ theme(axis.title.x = element_blank())+
  scale_x_continuous(limits = c(700,1800), breaks = seq(700,1800,100))+
  carbs + proteins + lipids + sporopollenin

p5 <- ggplot(data = rf.importance, aes(wavenumbers, rotundifolia))+
  geom_line()+ theme_bw()+ theme(axis.title.x = element_blank())+
  scale_x_continuous(limits = c(700,1800), breaks = seq(700,1800,100))+
  carbs + proteins + lipids + sporopollenin

p6 <- ggplot(data = rf.importance, aes(wavenumbers, suber))+
  geom_line()+ theme_bw()+ theme(axis.title.x = element_blank())+
  scale_x_continuous(limits = c(700,1800), breaks = seq(700,1800,100))+
  carbs + proteins + lipids + sporopollenin

p7 <- ggplot(data = rf.importance, aes(wavenumbers, mean))+
  geom_line()+ theme_bw()+ theme(axis.title.x = element_blank())+
  scale_x_continuous(limits = c(700,1800), breaks = seq(700,1800,100))+
  carbs + proteins + lipids + sporopollenin

grid.arrange(p1, p4, p2, p5, p3, p6, p7, nrow = 4)