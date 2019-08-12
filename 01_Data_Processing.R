library(here)
library(stringr)
source(here("R", "functions", "01_Data_proc.R"))

# Load Meta Info Dataframe
meta_info <- readRDS(here("Data", "Input", "data_metainfo.rds"))

# Load Spectra
spectra <- readRDS(here("Data", "Input", "Portugal_QuercusATR.rds"))

# Add spectra to data
meta_info$FTIR <- as.matrix(spectra[, 2:ncol(spectra)])
data = meta_info

rownames(data$FTIR) <- data$ID

### Savitzky Golay derivative
data$FTIR.SG2 <- SG_smooth(data$FTIR, deriv = 2)
data$FTIR.SG1 <- SG_smooth(data$FTIR, deriv = 1)
rownames(data$FTIR.SG1) <- data$ID
rownames(data$FTIR.SG2) <- data$ID

### Select six species of Quercus

sp.filter <- c("broteroi", "robur", "estremadurensis", "coccifera", "rotundifolia", "suber")
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
