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


pls.env_WC <- cppls(env_april_WC ~ FTIR.SG2, npc, data = subset, subset = train, scale =T)
pls.env_WC.test <- predict(pls.env_WC, newdata = subset[!subset$train,], type = "response")
plot(pls.env_WC.test[,1,5], subset$env_april_WC[!subset$train,1], asp = 1)
mean((subset$env_april_WC[!subset$train,1]-pls.env_WC.test[,1,5])^2)

subs <- subset(subset, Sub_Spec == "suber")
pls.env_WC <- cppls(env_april_WC[,2] ~ FTIR.SG2, npc, data = subs, subset = train, scale =T)
pls.env_WC.test <- predict(pls.env_WC, newdata = subs[!subs$train,], type = "response")
plot(pls.env_WC.test[,1,5], subs$env_april_WC[!subs$train,1], asp = 1)
cor((subs$env_april_WC[!subs$train,1]-pls.env_WC.test[,1,5]))

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
suber.train <- subset(subset, Sub_Spec == "suber" & train == TRUE)

pls.env.WS <- cppls(env_april_WS ~ FTIR.SG2, npc, data = suber.train, scale =T)
pls.env.WS.cv <- crossval(pls.env.WS, segments = 10)
pls.env.WS.test <- predict(pls.env.WS.cv, newdata = suber[!suber$train,], type = "response")

pls.env.WC <- cppls(env_april_WC ~ FTIR.SG2, npc, data = suber.train, scale =T)
pls.env.WC.cv <- crossval(pls.env.WC, segments = 10)
pls.env.WC.test <- predict(pls.env.WC.cv, newdata = suber[!suber$train,], type = "response")


plot(RMSEP(pls.env.WS.cv), main = "Weather station PLS")
plot(RMSEP(pls.env.WC.cv), main = "WorldClim PLS")

par(mfrow = c(2,3))
plot(suber$env_april_WS[!suber$train,1], pls.env.WS.test[,1,18], 
     main = "precipitation, 18 comp", xlab = "measured", ylab = "predicted", asp = 1)
plot(suber$env_april_WS[!suber$train,2], pls.env.WS.test[,2,22],
     main = "srad, 22 comp", xlab = "measured", ylab = "predicted", asp = 1)
plot(suber$env_april_WS[!suber$train,3], pls.env.WS.test[,3,38],
     main = "temperature, 38 comp", xlab = "measured", ylab = "predicted", asp = 1)
mtext("Weather stations PLS", side = 3, line = -1.5, outer = TRUE)

plot(suber$env_april_WC[!suber$train,1], pls.env.WC.test[,1,6], 
     main = "precipitation, 6 comp", xlab = "measured", ylab = "predicted", asp = 1)
plot(suber$env_april_WC[!suber$train,2], pls.env.WC.test[,2,6],
     main = "temperature, 6 comp", xlab = "measured", ylab = "predicted", asp = 1)
plot(suber$env_april_WC[!suber$train,3], pls.env.WC.test[,3,6],
     main = "srad, 6 comp", xlab = "measured", ylab = "predicted", asp = 1)
mtext("WorldClim PLS", side = 3, line = -22, outer = TRUE)

plot(pls.env.WS.cv, ncomp = c(18,22,38), line = T)
plot(pls.env.WC.cv, ncomp = 6, line = T)

# env

north <- subset(subset, Group == "Porto" | Group == "Horta da Vilarica" | Group == "Alijo" | Group == "Freixo de Espada A Cinta" | Group == "Peso da Regua")
pls.north <- cppls(env_april_WC ~ FTIR, 100, data = north, scale =T)
pls.north.cv <- crossval(pls.north, segments = 10)
plot(RMSEP(pls.north.cv))

coast <- subset(subset, Group != "Porto" & Group != "Horta da Vilarica" & Group != "Alijo" & Group != "Freixo de Espada A Cinta" & Group != "Peso da Regua")
coast <- subset(coast, Species == "suber")
pls.coast <- cppls(env_april_WC ~ FTIR, 100, data = coast, scale =T)
pls.coast.cv <- crossval(pls.coast, segments = 10)
plot(RMSEP(pls.coast.cv))
plot(pls.coast.cv, ncomp = 10)

save(pls.train, pls.test, pls.train.yadd, pls.test.yadd, plsr.train, plsr.test, pls.env, file = "pls.rda")

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
  colnames(ldngs.peaks)[1:10] <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")
  ldngs.df <- melt(ldngs, id = "wavenumber")
  colnames(ldngs.df)<-c("Wavenumber", "PC", "SG2")
  return(list(ldngs.df,ldngs.peaks))
}

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

PLS.plot.data<- data.frame(Species = factor(pls.eval$fitdata$Species),
                           PC1 = pls.eval$fitdata$FTIR.score[,1],
                           PC2 = pls.eval$fitdata$FTIR.score[,2],
                           PC3 = pls.eval$fitdata$FTIR.score[,3],
                           PC4 = pls.eval$fitdata$FTIR.score[,4],
                           PC5 = pls.eval$fitdata$FTIR.score[,5])
plot <- ggplot(PLS.plot.data,aes(PC1,PC2, color = Species)) +
  geom_point() +
  scale_color_discrete() + stat_ellipse() +
  theme_bw() + theme(text = element_text(size = 18))



#Crossvalidated
pls <- cppls(Species.HO ~ msc(FTIR), npc, data = subset[subset$train,], scale = T)
pls.cv <- crossval(pls, segments = 10)
pls.cv.test <- predict(pls.cv, newdata = subset[!subset$train,], type = "score")
pls.cv.eval <- evaluate_pls(npc, pls.cv, pls.cv.test, subset)

pc_plots <- function(plot, loadings, alpha = 1) {
  require(ggplot2)
  plot1 <- ggplot(plot,aes(PC1,PC2, color = Species)) +
    geom_point(alpha = alpha) + coord_equal() +
    scale_color_discrete() + stat_ellipse() +
    theme_bw() + theme(text = element_text(size = 18))
  plot1_ldg <- plot1 + geom_segment(data=loadings, aes(x=0, xend=PC1*100, y=0, yend=PC2*100),
                                    color = "red", arrow=arrow(length=unit(0.05,"npc"))) +
    geom_text(data=loadings, aes(x=PC1*100,y=PC2*100,label=loadings$Comp,
                                 hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), color = "black", size=6)
  plot2 <- ggplot(plot,aes(PC3,PC4, color = Species)) +
    geom_point(alpha = alpha) + coord_equal() +
    scale_color_discrete() + stat_ellipse() +
    theme_bw() + theme(text = element_text(size = 18))
  plot2_ldg <- plot2 + geom_segment(data=loadings, aes(x=0, xend=PC3*100, y=0, yend=PC4*100),
                                    color = "red", arrow=arrow(length=unit(0.05,"npc"))) +
    geom_text(data=loadings, aes(x=PC3*100,y=PC4*100,label=loadings$Comp,
                                 hjust=0.5*(1-sign(PC3)),vjust=0.5*(1-sign(PC4))), color = "black", size=6)
  results <- list(plot1, plot1_ldg, plot2, plot2_ldg)
  return(results)
}

PLS.plot.data<- data.frame(Species = factor(pls.cv.eval$fitdata$Species),
                           PC1 = pls.cv.eval$fitdata$FTIR.score[,1],
                           PC2 = pls.cv.eval$fitdata$FTIR.score[,2],
                           PC3 = pls.cv.eval$fitdata$FTIR.score[,3],
                           PC4 = pls.cv.eval$fitdata$FTIR.score[,4],
                           PC5 = pls.cv.eval$fitdata$FTIR.score[,5])

pls.ldgs <- ldgs.data(pls.cv)[[2]]
pls.cv.plots <- pc_plots(PLS.plot.data, pls.ldgs)

loadings <- data.frame(wavenumbers = as.numeric(row.names(pls.cv$loadings)),
                       PC1 = pls.cv$loading.weights[,1],
                       PC2 = pls.cv$loading.weights[,2],
                       PC3 = pls.cv$loading.weights[,3],
                       PC4 = pls.cv$loading.weights[,4],
                       PC5 = pls.cv$loading.weights[,5])
ggplot(data = loadings, aes(wavenumbers, PC1)) + geom_line(alpha = .5) + sporopollenin + lipids + theme_bw()
plot(as.numeric(row.names(pls.cv$loading.weights)), pls.cv$loading.weights[,1], type = "l")

ilex <- subset(subset, Section == "Ilex")
ilex_pls <- cppls(Species.HO ~ FTIR.SG2, npc, data = ilex[ilex$train,], scale = T)
ilex_pls.cv <- crossval(ilex_pls, segments = 10)
ilex_pls.cv.test <- predict(ilex_pls.cv, newdata = ilex[!ilex$train,], type = "score")
ilex_pls.cv.eval <- evaluate_pls(npc, ilex_pls.cv, ilex_pls.cv.test, ilex)

PLS.plot.data<- data.frame(Species = factor(ilex_pls.cv.eval$fitdata$Species),
                           PC1 = ilex_pls.cv.eval$fitdata$FTIR.score[,1],
                           PC2 = ilex_pls.cv.eval$fitdata$FTIR.score[,2],
                           PC3 = ilex_pls.cv.eval$fitdata$FTIR.score[,3],
                           PC4 = ilex_pls.cv.eval$fitdata$FTIR.score[,4],
                           PC5 = ilex_pls.cv.eval$fitdata$FTIR.score[,5])

ord_plot_1_2 <- ggplot(PLS.plot.data,aes(PC1,PC2, color = Species)) +
  geom_point() + coord_equal() +
  scale_color_discrete() + stat_ellipse() +
  theme_bw() + theme(text = element_text(size = 18))

suber <- subset(subset, Section == "Cerris")
suber <- subset(suber, Group == "Porto" | Group == "Arrabida" | Group == "Alijo" | Group == "Coimbra" | Group == "Candeeiros")
suber$Group.H0<-I(model.matrix(~y-1, data.frame(y = factor(as.character(suber$Group)))))
suber_pls <- cppls(env_april_WC ~ FTIR.SG2, npc, data = suber, scale = T)
suber_pls.cv <- crossval(suber_pls, segments = 10)
suber_pls.cv.test <- predict(suber_pls.cv, newdata = suber[!suber$train,], type = "score")
suber_pls.cv.eval <- evaluate_pls(npc, suber_pls.cv, suber_pls.cv.test, suber)

suber_pls.plot.data<- data.frame(Group = suber[,"Group"],
                                 Location = suber[,"Location"],
                           PC1 = suber_pls.cv$scores[,1],
                           PC2 = suber_pls.cv$scores[,2],
                           PC3 = suber_pls.cv$scores[,3],
                           PC4 = suber_pls.cv$scores[,4],
                           PC5 = suber_pls.cv$scores[,5])

suber_pls_pc_1_2 <- ggplot(suber_pls.plot.data,aes(PC1,PC2, color = Group)) +
  geom_point(size = 3, alpha = .1) + coord_equal() +
  scale_color_discrete() + stat_ellipse() +
  theme_bw() + theme(text = element_text(size = 18)) +
  geom_point(data = subset(suber_pls.plot.data, Group == "Coimbra"), aes(PC1, PC2, color = Location), size = 3)

sub_pls <- cppls(Species.HO ~ FTIR.SG2, npc, data = subset, scale = T)
sub_pls.cv <- crossval(sub_pls, segments = 10)

PLS.plot.data<- data.frame(Species = subset$Sub_Spec,
                           Group = subset$Group,
                           Location = subset$Location,
                           PC1 = sub_pls.cv$scores[,1],
                           PC2 = sub_pls.cv$scores[,2],
                           PC3 = sub_pls.cv$scores[,3],
                           PC4 = sub_pls.cv$scores[,4],
                           PC5 = sub_pls.cv$scores[,5])

ord_plot_1_2 <- ggplot(PLS.plot.data,aes(PC1,PC2, color = Species)) +
  geom_point(alpha = .1) + coord_equal() +
  scale_color_discrete() + stat_ellipse() +
  theme_bw() + theme(text = element_text(size = 18)) +
  geom_point(data = subset(PLS.plot.data, Group == "Alijo"  & Species == "suber" | Group == "Candeeiros" & Species == "suber"), aes(PC1, PC2, color = Group), size = 3)

table <- subset %>%
  group_by(Sub_Spec) %>%
  summarise(min_T = min(april_temp_WC), max_T = max(april_temp_WC))
  
