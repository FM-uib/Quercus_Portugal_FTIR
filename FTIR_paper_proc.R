library(ggplot2)
library(stringr)
library(EMSC)
library(plyr)

setwd("O:/PhD/Data/Portugal 2018/paper")

load(file = "joined.rda")
load(file = "elev_precip.rda")

### Savitzky Golay derivative
SG2 <- SavitzkyGolay(joined$FTIR)
SG1 <- SavitzkyGolay(joined$FTIR, deriv = 1)

colnames(SG2) <- colnames(joined$FTIR)
rownames(SG2) <- rownames(joined$FTIR)

colnames(SG1) <- colnames(joined$FTIR)
rownames(SG1) <- rownames(joined$FTIR)

joined$FTIR.SG2 <- SG2
joined$FTIR.SG1 <- SG1

### Subset Dataset
sp.filter <- c("coccifera", "broteroi", "robur", "estremadurensis", "rotundifolia", "suber")
subset <- joined[joined$Sub_Spec %in% sp.filter,]

### Test Valid Split by sample replicate across locations
set.seed(12345)
subset$ID <- levels(factor(subset$ID))
# 60/40 split
n <- length(levels(factor(subset$ID)))
id <- sample(levels(factor(subset$ID)), signif(n*.6, 2)) # randomly draw Samples (all replicates)

# split dataset into test and train
train <- subset[subset$ID %in% id,] # alternatively filter() but can't subset the matrixes train <- filter(joined[,1:7], ID %in% id)
test <- subset[!subset$ID %in% id,]

subset$train <- TRUE
subset[rownames(test),"train"] <- FALSE
subset$Species.HO <- I(model.matrix(~Sub_Spec-1, subset))
colnames(subset$Species.HO) <- str_sub(colnames(subset$Species.HO), 9)
subset <- join(subset, elev_precip, type="inner", by = "SID")
subset$env <- I(as.matrix(subset[,20:22]))

subset$FTIR.SG2 <- subset$FTIR.SG2[,-c(1:5,620:624)]

save(train, test, subset, file = "data.rda")
