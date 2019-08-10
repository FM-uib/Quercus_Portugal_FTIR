library(here)
library(stringr)
source(here("R", "functions", "01_Data_proc.R"))

# Load Meta Info Dataframe
meta_info <- readRDS(here("Data", "Input", "meta_info.rds"))

# Load Spectra
spectra <- read.csv(here("Data","Input","Quercus_ATRportugal.csv"), sep= ";")
colnames(spectra)<-c("SID", str_sub(colnames(spectra)[-1],start = 2))

# Sort spectra and Meta Info
meta_info <- meta_info[order(meta_info$SID),]
spectra <- spectra[order(spectra$SID),]

# Select Samples with measured spectra
data <- meta_info[meta_info$SID %in% spectra$SID,]

# Add spectra to data
data$FTIR <- as.matrix(spectra[, 2:ncol(spectra)])

rownames(data$FTIR) <- data$ID

### Savitzky Golay derivative
data$FTIR.SG2 <- SG_smooth(data$FTIR, deriv = 2)
data$FTIR.SG1 <- SG_smooth(data$FTIR, deriv = 1)
rownames(data$FTIR.SG1) <- data$ID
rownames(data$FTIR.SG2) <- data$ID

### Select six species of Quercus
levels(data$Sub_Spec)[1] = "faginea"
sp.filter <- c("faginea", "robur", "estremadurensis", "coccifera", "rotundifolia", "suber")
data <- data[data$Sub_Spec %in% sp.filter,]

### Mean the spectra by measurement
data_mean <- data[data$MRep == "03", c(1:14)]
data_mean$FTIR = I(mean_spectra(data$FTIR, data$ID))
data_mean$FTIR.SG2 = I(mean_spectra(data$FTIR.SG2, data$ID))
data_mean$FTIR.SG2 = data_mean$FTIR.SG2[,-c(1:5,(ncol(data_mean$FTIR.SG2)-4):ncol(data_mean$FTIR.SG2))]

### Create response matrix for pls
data_mean$Sub_Spec <- factor(data_mean$Sub_Spec, levels = sp.filter)
data_mean$Species.HO <- I(model.matrix(~Sub_Spec-1, data_mean))
colnames(data_mean$Species.HO) <- str_sub(colnames(data_mean$Species.HO), 9)

saveRDS(data, file = here("Data", "Output", "data.rds"))
saveRDS(data_mean, file = here("Data", "Output", "data_mean.rds"))
