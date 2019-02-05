library(pls)
library(ggplot2)
library(MASS)
library(here)
library(caret)
setwd("O:/PhD/Data/Portugal 2018/paper")

load(file = here("Data","Input","data_WC.rda"))
subset <- readRDS(file = here("Data","Input","data_WC.rds"))

S.regions <- c(145:154,191:200,371:380,546:555)
subset$SG2.S.R <- I(subset$FTIR.SG2[,S.regions])

# Predict CPLS scores for test data
npc = 100
pls.train <- cppls(Species.HO ~ FTIR.SG2, npc, data = subset, subset = train, scale=T)
pls.test <- predict(pls.train, newdata = subset[!subset$train,], type = "score")
pls.eval <- evaluate_pls(npc, pls.train, pls.test, subset)

pls.WS.yadd.train <- cppls(Species.HO ~ FTIR.SG2 + env_april_WS ,npc, data = subset, subset = train, scale=T)
pls.WS.yadd.test <- predict(pls.WS.yadd.train, newdata = subset[!subset$train,], type = "score")
pls.WS.eval <- evaluate_pls(npc, pls.WS.yadd.train, pls.WS.yadd.test, subset)

pls.WC.yadd.train <- cppls(Species.HO ~ FTIR.SG2 + env_april_WC ,npc, data = subset, subset = train, scale=T)
pls.WC.yadd.test <- predict(pls.WC.yadd.train, newdata = subset[!subset$train,], type = "score")
pls.WC.eval <- evaluate_pls(npc, pls.WC.yadd.train, pls.WC.yadd.test, subset)

pls.env.yadd.train <- cppls(Species.HO ~ FTIR.SG2 + env ,npc, data = subset, subset = train, scale=T)
pls.env.yadd.test <- predict(pls.env.yadd.train, newdata = subset[!subset$train,], type = "score")
pls.env.eval <- evaluate_pls(npc, pls.env.yadd.train, pls.env.yadd.test, subset)

#plsr.train <- plsr(Species.HO ~ FTIR.SG2 , npc, data = subset, subset = train, scale=T)
#plsr.test <- predict(plsr.train, newdata = subset[!subset$train,], type = "score")

#grepl("pls\\.", ls())

error.df<-data.frame(pc = c(1:npc),
                     CPLS = pls.eval$error, 
                     CPLS.WS = pls.WS.eval$error, 
                     CPLS.WC = pls.WC.eval$error,
                     CPLS.env = pls.env.eval$error)
error.df<-melt(error.df,id="pc")
colnames(error.df)<-c("Components", "Model", "RMSEP")
ggplot(error.df, aes(Components, RMSEP , colour=Model)) +
  geom_line() + theme_bw() + theme(text = element_text(size = 18))



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
pls.env.WC <- cppls(env_april_WS ~ FTIR, npc, data = suber, subset = train, scale =T)
pls.env.WC.test <- predict(pls.env.WC, newdata = suber[!suber$train,], type = "response")
plot(pls.env.WS.test[,1,5], suber$env_april_WS[!suber$train,1], asp = 1)

pls.env.ftir <- cppls(env ~ FTIR, npc, data = suber, subset = train, scale =T)
pls.env.ftir.test <- predict(pls.env.ftir, newdata = suber[!suber$train,], type = "response")
plot(pls.env.ftir.test[,1,5], suber$env[!suber$train,1], asp = 1)

suber$Group.H0<-I(model.matrix(~y-1, data.frame(y = factor(suber$Group))))
pls.pop <- cppls(Group.H0 ~ FTIR.SG2, npc, data = suber, subset = train, scale =T)
pls.pop.test <- predict(pls.pop, newdata = suber[!suber$train,], type = "response")

robur <- subset(subset, Sub_Spec == "robur")
pls.env <- cppls(env ~ FTIR.SG2, npc, data = robur, subset = train, scale =T)
pls.env.test <- predict(pls.env, newdata = robur[!robur$train,], type = "response")
plot(pls.env.test[,1,5], robur$mean_temp[!robur$train], asp = 1)

save(pls.train, pls.test, pls.train.yadd, pls.test.yadd, plsr.train, plsr.test, pls.env, file = "pls.rda")

evaluate_pls <- function(npc, trained, tested, data){
  error <- rep(0, npc)
  con_M <- list()
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
}

evaluate_pls2 <- function(npc, trained, tested, data){
  error <- rep(0, npc)
  con_M <- list()
  for (i in 1:npc) {
    fitdata <- data.frame(Group = data$Group[data$train], 
                          FTIR.score = I(trained$scores[,1:i, drop = FALSE]))
    testdata <- data.frame(Group = data$Group[!data$train],
                           FTIR.score = I(tested[,1:i,drop=FALSE]))
    error[i] <- (nrow(tested) - sum(predict(lda(Group ~ FTIR.score, data = fitdata),
                                            newdata = testdata)$class == testdata$Group))/nrow(tested)
    con_M[[i]] <- I(confusionMatrix(predict(lda(Group ~ FTIR.score, data = fitdata), 
                                            newdata = testdata)$class, testdata$Group))
  }
  result <- list("error" = error, "conf_matrix" = con_M, "fitdata" = fitdata, "testdata" = testdata)
  return(result)
}

error.df<-data.frame(pc = c(1:30),CPLS = error[1,], CPLS.env = error[2,], PLSR = error[3,])
error.df<-melt(error.df,id="pc")
colnames(error.df)<-c("Components", "Model", "RMSEP")
ggplot(error.df, aes(Components, RMSEP , colour=Model)) +
  geom_line() + theme_bw() + theme(text = element_text(size = 18))

PLS.plot.data<- data.frame(Species = factor(fitdata2$Species),
                           PC1 = fitdata2$FTIR.score[,1],
                           PC2 = fitdata2$FTIR.score[,2],
                           PC3 = fitdata2$FTIR.score[,3],
                           PC4 = fitdata2$FTIR.score[,4],
                           PC5 = fitdata2$FTIR.score[,5])
plot <- ggplot(PLS.plot.data,aes(PC1,PC2, color = Species)) +
  geom_point() +
  scale_color_discrete() + stat_ellipse() +
  theme_bw() + theme(text = element_text(size = 18))
