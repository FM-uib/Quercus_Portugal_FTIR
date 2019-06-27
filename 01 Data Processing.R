library(here)
library(stringr)

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
SG_smooth <- function(spectra, deriv){
  library(EMSC)
  res = SavitzkyGolay(spectra, deriv = deriv)
  colnames(res) = colnames(spectra)
  rownames(res) = rownames(spectra)
  return(res)
}

data$FTIR.SG2 <- SG_smooth(data$FTIR, deriv = 2)
data$FTIR.SG1 <- SG_smooth(data$FTIR, deriv = 1)

### Select six species of Quercus and create matrix for pls
sp.filter <- c("broteroi", "robur", "estremadurensis", "coccifera", "rotundifolia", "suber")
data <- data[data$Sub_Spec %in% sp.filter,]

data$Sub_Spec <- factor(data$Sub_Spec, levels = sp.filter)
data$Species.HO <- I(model.matrix(~Sub_Spec-1, data))
colnames(data$Species.HO) <- str_sub(colnames(data$Species.HO), 9)
