library(pls)
library(ggplot2)
library(MASS)
setwd("O:/PhD/Data/Portugal 2018/paper")

load(file = "data.rda")
S.regions <- c(145:154,191:200,371:380,546:555)
subset$SG2.S.R <- I(subset$FTIR.SG2[,S.regions])
L.regions <- c(74:83,218:227,603:612)
subset$SG2.L.R <- I(subset$FTIR.SG2[,L.regions])
P.regions <- c(120:129,173:182)
subset$SG2.P.R <- I(subset$FTIR.SG2[,P.regions])
C.regions <- c(403:412,420:429,430:439,444:453,462:471)
subset$SG2.C.R <- I(subset$FTIR.SG2[,C.regions])
subset$Section.HO <- I(model.matrix(~y-1, data.frame(y = factor(subset$Section))))

npc = 30

S.regions.train <- cppls(Species.HO ~ SG2.S.R, npc, data = subset, subset = train, scale=T)
S.regions.test <-  predict(S.regions.train, newdata = subset[!subset$train,], type = "score")

L.regions.train <- cppls(Species.HO ~ SG2.L.R + SG2.P.R + SG2.C.R, npc, data = subset, subset = train, scale=T)
L.regions.test <- predict(L.regions.train, newdata = subset[!subset$train,], type = "score")

S.L.C.P.train <- plsr(Species.HO ~ SG2.L.R + SG2.S.R + SG2.P.R + SG2.C.R, npc, data = subset, subset = train, scale=T)
S.L.C.P.test <- predict(S.L.C.P.train, newdata = subset[!subset$train,], type = "score")

S.env.train <- cppls(Species.HO ~ SG2.S.R + env, npc, data = subset, subset = train, scale=T)
S.env.test <- predict(S.env.train, newdata = subset[!subset$train,], type = "score")

L.env.train <- cppls(Species.HO ~ SG2.L.R + SG2.P.R + SG2.C.R + env, npc, data = subset, subset = train, scale=T)
L.env.test <- predict(L.env.train, newdata = subset[!subset$train,], type = "score")

S.L.train <- cppls(Species.HO ~ SG2.L.R + SG2.S.R, npc, data = subset, subset = train, scale=T)
S.L.test <- predict(S.L.train, newdata = subset[!subset$train,], type = "score")

FTIR.train <- cppls(Species.HO ~ FTIR.SG2, npc, data = subset, subset = train, scale=T)
FTIR.test <- predict(FTIR.train, newdata = subset[!subset$train,], type = "score")

error <- matrix(ncol = npc, nrow = 7)
dimnames(error) <- list(Model = c('S', 'L', 'S_L_C_P', 'S_env', 'L_env', 'S_L', 'full'), ncomp = 1:npc)

calc.error <- function(data, train, test, error, i, j){
  n <- nrow(test)
  fit <- data.frame(Species = data$Sub_Spec[data$train],
                    FTIR.score = I(train$scores[,1:i,drop=FALSE]))
  test <- data.frame(Species = data$Sub_Spec[!data$train],
                          FTIR.score = I(test[,1:i,drop=FALSE]))
  error[j,i] <- (n - sum(predict(lda(Species ~ FTIR.score, data = fit),
                                          newdata = test)$class == test$Species)) /n
  return(error)
}

for (i in 1:npc) {
  error <- calc.error(subset, S.regions.train, S.regions.test, error, i = i, j = 1)
  error <- calc.error(subset, L.regions.train, L.regions.test, error, i = i, j = 2)
  error <- calc.error(subset, S.L.C.P.train, S.L.C.P.test, error, i = i, j = 3)
  error <- calc.error(subset, S.env.train, S.env.test, error, i = i, j = 4)
  error <- calc.error(subset, L.env.train, L.env.test, error, i = i, j = 5)
  error <- calc.error(subset, S.L.train, S.L.test, error, i = i, j = 6)
  error <- calc.error(subset, FTIR.train, FTIR.test, error, i = i, j = 7)
}
round(error,2)

error.df<-data.frame(pc = c(1:30),
                     S = error[1,], 
                     L_C_P = error[2,],
                     S_L_C_P = error[3,],
                     S.env = error[4,],
                     L_C_P.env = error[5,],
                     #S_L = error[6,],
                     full = error[7,])
error.df<-melt(error.df,id="pc")
colnames(error.df)<-c("Components", "Model", "RMSEP")
ggplot(error.df, aes(Components, RMSEP , colour=Model))+
  geom_line()+theme_bw()+theme(text = element_text(size = 18))

# Section Class

S.regions.train <- cppls(Section.HO ~ SG2.S.R, npc, data = subset, subset = train, scale=T)
S.regions.test <-  predict(S.regions.train, newdata = subset[!subset$train,], type = "score")

