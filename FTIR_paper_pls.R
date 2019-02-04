library(pls)
library(ggplot2)
library(MASS)
library(here)
setwd("O:/PhD/Data/Portugal 2018/paper")

load(file = here("Data","Input","data_WC.rda"))
S.regions <- c(145:154,191:200,371:380,546:555)
subset$SG2.S.R <- I(subset$FTIR.SG2[,S.regions])

# Predict CPLS scores for test data
npc = 30
pls.train <- cppls(Species.HO ~ FTIR.SG2, npc, data = subset, subset = train, scale=T)
pls.test <- predict(pls.train, newdata = subset[!subset$train,], type = "score")

pls.train.yadd <- cppls(Species.HO ~ FTIR.SG2 + env_april_WS ,npc, data = subset, subset = train, scale=T)
pls.test.yadd <- predict(pls.train.yadd, newdata = subset[!subset$train,], type = "score")

plsr.train <- plsr(Species.HO ~ FTIR.SG2 , npc, data = subset, subset = train, scale=T)
plsr.test <- predict(plsr.train, newdata = subset[!subset$train,], type = "score")

# Using S-regions
pls.train <- cppls(Species.HO ~ SG2.S.R, npc, data = subset, subset = train, scale=T)
pls.test <- predict(pls.train, newdata = subset[!subset$train,], type = "score")

pls.train.yadd <- cppls(Species.HO ~ SG2.S.R + env ,npc, data = subset, subset = train, scale=T)
pls.test.yadd <- predict(pls.train.yadd, newdata = subset[!subset$train,], type = "score")

plsr.train <- cppls(Species.HO ~ FTIR.SG2 + env, npc, data = subset, subset = train, scale=T)
plsr.test <- predict(plsr.train, newdata = subset[!subset$train,], type = "score")

pls.env <- cppls(env ~ FTIR.SG2, npc, data = subset, subset = train, scale =T)
pls.env.test <- predict(pls.env, newdata = subset[!subset$train,], type = "response")
plot(pls.env.test[,1,5], subset$mean_temp[!subset$train], asp = 1)
mean((subset$mean_temp[!subset$train]-pls.env.test[,1,5])^2)

# Only suber for env modelling
suber <- subset(subset, Sub_Spec == "suber")
pls.env <- cppls(env_april_WC ~ FTIR.SG2, npc, data = suber, subset = train, scale =T)
pls.env.test <- predict(pls.env, newdata = suber[!suber$train,], type = "response")
plot(pls.env.test[,1,5], suber$env_april_WS[!suber$train,1], asp = 1)

robur <- subset(subset, Sub_Spec == "robur")
pls.env <- cppls(env ~ FTIR.SG2, npc, data = robur, subset = train, scale =T)
pls.env.test <- predict(pls.env, newdata = robur[!robur$train,], type = "response")
plot(pls.env.test[,1,5], robur$mean_temp[!robur$train], asp = 1)

save(pls.train, pls.test, pls.train.yadd, pls.test.yadd, plsr.train, plsr.test, pls.env, file = "pls.rda")

error <- matrix(ncol = npc, nrow = 3)
dimnames(error) <- list(Model = c('CPLS', 'CPLS (Y.add)', 'PLSR'), ncomp = 1:npc)

for (i in 1:npc) {
  fitdata1 <- data.frame(Species = subset$Sub_Spec[subset$train],
                         FTIR.score = I(pls.train$scores[,1:i,drop=FALSE]))
  testdata1 <- data.frame(Species = subset$Sub_Spec[!subset$train],
                          FTIR.score = I(pls.test[,1:i,drop=FALSE]))
  error[1,i] <-
    (342 - sum(predict(lda(Species ~ FTIR.score, data = fitdata1),
                       newdata = testdata1)$class == testdata1$Species)) / 342
  
  fitdata2 <- data.frame(Species = subset$Sub_Spec[subset$train],
                         FTIR.score = I(pls.train.yadd$scores[,1:i,drop=FALSE]))
  testdata2 <- data.frame(Species = subset$Sub_Spec[!subset$train],
                          FTIR.score = I(pls.test.yadd[,1:i,drop=FALSE]))
  error[2,i] <-
    (342 - sum(predict(lda(Species ~ FTIR.score, data = fitdata2),
                       newdata = testdata2)$class == testdata2$Species)) / 342
  
  fitdata3 <- data.frame(Species = subset$Sub_Spec[subset$train],
                         FTIR.score = I(plsr.train$scores[,1:i,drop=FALSE]))
  testdata3 <- data.frame(Species = subset$Sub_Spec[!subset$train],
                          FTIR.score = I(plsr.test[,1:i,drop=FALSE]))
  error[3,i] <-
    (342 - sum(predict(lda(Species ~ FTIR.score, data = fitdata3),
                       newdata = testdata3)$class == testdata3$Species)) / 342
}
round(error,2)

error.df<-data.frame(pc = c(1:30),CPLS = error[1,], CPLS.env = error[2,], PLSR = error[3,])
error.df<-melt(error.df,id="pc")
colnames(error.df)<-c("Components", "Model", "RMSEP")
ggplot(error.df, aes(Components, RMSEP , colour=Model))+
  geom_line()+theme_bw()+theme(text = element_text(size = 18))

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

PLS.plot.data<- data.frame(Species = factor(fitdata2$Species),
                           PC1 = fitdata2$FTIR.score[,1],
                           PC2 = fitdata2$FTIR.score[,2],
                           PC3 = fitdata2$FTIR.score[,3],
                           PC4 = fitdata2$FTIR.score[,4],
                           PC5 = fitdata2$FTIR.score[,5])
plot <- ggplot(PLS.plot.data,aes(PC1,PC2, color = Species))+
  geom_point()+
  scale_color_discrete()+stat_ellipse()+
  theme_bw()+theme(text = element_text(size = 18))
