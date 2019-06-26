library(here)
library(stringr)

# Load Meta Info Dataframe
load(here("Data","Input", "samples.df.rda"))
# Load Spectra
spectra <- read.csv(here("Data","Input","Quercus_ATRportugal.csv"), sep= ";")

colnames(spectra)<-c("SID", str_sub(colnames(spectra)[-1],start = 2))

samples.df<-samples.df[,1:13]

# Sort spectra and Meta Info
data <- data[order(data$SID),]
spectra <- spectra[order(spectra$SID),]

# Select Samples with measured spectra
data <- samples.df[samples.df$SID %in% spectra$SID,]

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

sp.filter <- c("coccifera", "broteroi", "robur", "estremadurensis", "rotundifolia", "suber")
data <- data[data$Sub_Spec %in% sp.filter,]

data$Species.HO <- I(model.matrix(~Sub_Spec-1, data))
colnames(data$Species.HO) <- str_sub(colnames(data$Species.HO), 9)
