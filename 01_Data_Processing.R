library(here)
library(stringr)
source(here("R", "functions", "01_Data_proc.R"))

# Load Meta Info Dataframe
meta_info <- read.csv(here("Data", "Input", "data_metainfo.csv"))
meta_info = meta_info[order(meta_info$SID),]

# Select Quercus Species for analysis
sp.filter <- c("broteroi", "robur", "estremadurensis", "coccifera", "rotundifolia", "suber")
data <- meta_info[meta_info$Sub_Spec %in% sp.filter,]
data$Sub_Spec <- factor(data$Sub_Spec, levels = sp.filter)

# Load Spectra
spectra_raw <- read.csv(here("Data", "Input", "Quercus_ATR_RAW.csv"), check.names = F)
spectra_raw$samples = sapply(spectra_raw$Filename, str_sub, start = -13L , end = -3L)
spectra_raw = spectra_raw[order(spectra_raw$samples),]

# Select spectra for analysis
spectra_raw = spectra_raw[spectra_raw$samples %in% data$SID,]

spectra_matrix = as.matrix(spectra_raw[,1:3318])
wavenumbers = as.numeric(colnames(spectra_matrix))
ftir = spectra_matrix[,wavenumbers <= 1900 & wavenumbers >= 700]
ftir_sg = spectral_processing(spectra_matrix)

# Add spectra to data
data$ftir = ftir
data$ftir_sg = ftir_sg

### Mean the spectra by measurement
data_mean <- data[data$MRep == 03, c(2:15)]
data_mean$ftir = I(mean_spectra(data$ftir, data$ID))
data_mean$ftir_sg = I(mean_spectra(data$ftir_sg, data$ID))
colnames(data_mean$ftir_sg) = colnames(data_mean$ftir)

### Create response matrix for pls
data_mean$Species.HO <- I(model.matrix(~Sub_Spec-1, data_mean))
colnames(data_mean$Species.HO) <- str_sub(colnames(data_mean$Species.HO), 9)

saveRDS(data, file = here("Data", "Output", "data.rds"))
saveRDS(data_mean, file = here("Data", "Output", "data_mean.rds"))
