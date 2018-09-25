library(reshape2)
library(plyr)
setwd("O:/PhD/Data/Portugal 2018")
load("Samples.rda")
my.summary <- function(x, na.rm=TRUE){result <- c(Samples=as.integer(nrow(x)),
                                                  Q.rob = sum(x$Sub_Spec == "robur"),
                                                  Q.fag  = sum(x$Sub_Spec == "broteroi"),
                                                  Q.rot = sum(x$Sub_Spec == "rotundifolia"),
                                                  Q.coc  = sum(x$Sub_Spec == "coccifera"),
                                                  Q.sub = sum(x$Sub_Spec == "suber"),
                                                  Q.r.est = sum(x$Sub_Spec =="estremadurensis"),
                                                  Q.fagx = sum(x$Species == "faginea" & !x$Sub_Spec == "broteroi"),
                                                  Q.lus  = sum(x$Sub_Spec == "lusitanica"))}

red.summary <- function(x, na.rm=TRUE){result <- c(Samples=as.integer(nrow(x)),
                                                  Q.rob = sum(x$Sub_Spec == "robur"),
                                                  Q.fag  = sum(x$Sub_Spec == "broteroi"),
                                                  Q.rot = sum(x$Sub_Spec == "rotundifolia"),
                                                  Q.coc  = sum(x$Sub_Spec == "coccifera"),
                                                  Q.sub = sum(x$Sub_Spec == "suber"))}

Portugal.melt<-melt(Portugal[,c(3,4,5,11,12)])

SumTable.GroupXLoc <- ddply(Portugal.melt, c("Group","Location"), function(x) my.summary(x))
SumTable.Group <- ddply(Portugal.melt, c("Group"), function(x) my.summary(x))

SumTable.red <- ddply(Portugal.melt, c("Group"), function(x) red.summary(x))
SumTable.red$Samples<-sapply(c(1:nrow(SumTable.Group)),function(x) sum(SumTable.red[x,3:7]))

SumTable.GroupXLoc.red <- ddply(Portugal.melt, c("Group","Location"), 
                                function(x) red.summary(x))
SumTable.GroupXLoc.red$Samples<-sapply(c(1:nrow(SumTable.GroupXLoc.red)),
                                       function(x) sum(SumTable.GroupXLoc.red[x,4:8]))
