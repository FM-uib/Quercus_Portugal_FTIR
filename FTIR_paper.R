library(ggplot2)
library(stringr)
library(EMSC)
library(plyr)

setwd("O:/PhD/Data/Portugal 2018/paper")
FTIR.data<-read.csv("Quercus ATRportugal.csv", sep= ";")
load("Samples.rda")
load("Samples.df.rda")

colnames(FTIR.data)<-c("SamplesName", str_sub(colnames(FTIR.data)[-1],start = 2))

samples.df<-samples.df[,1:13]
FTIR.data$SID<-as.character(FTIR.data$SamplesName)
FTIR<-as.matrix(FTIR.data[,2:625])
row.names(FTIR)<-FTIR.data$SID

codes<-FTIR.data[,c(1,626)]
joined<-join(codes, samples.df, type="inner", by = "SID")
FTIR<-as.matrix(FTIR.data[,2:625])
joined$FTIR<-FTIR

save(joined,file="joined.rda")
save(FTIR.data, file = "FTIR.df")
