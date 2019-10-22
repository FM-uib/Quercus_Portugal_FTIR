library(here)
library(EMSC)

raw = read.csv(here("Data_safe","Input","Quercus_ATR_RAW.csv"), check.names = F)
spectra_raw = as.matrix(raw[,1:3318])

spectral_processing <- function(spectra, poly = 2, width = 11){
  require(EMSC)
  # The attached file should contain the correctly pre-processed spectra:
  #1)	SG 2nd derivative (ws 11, polynomial 2) on the whole dataset
  #2)	Selection of spectral sub-region of interest (1900-700 cm-1)
  #3)	MSC
  #
  #It’s important that (1) is done before (3).
  
  sec_degreee = SavitzkyGolay(spectra, poly = poly, width = width)
  
}

c(1:10) > 3 & c(1:10) < 8
tmp = SavitzkyGolay(spectra_raw,2,11,2)
wavenumbers = as.numeric(colnames(spectra_raw))
colnames(tmp) = as.numeric(colnames(spectra_raw))
tmp = tmp[,wavenumbers <= 1900 & wavenumbers >= 700]

replicates = sapply(raw$Filename, str_sub, start = -13L , end = -6L)

tmp_emsc = EMSC(tmp, degree = 6, replicates = replicates, rep_corr = .9)

boris = read.csv(here("Data","Input","Quercus_ATRportugal2ndDerMSC.csv"), check.names = F)


plot(colnames(boris)[-1],boris[1,-1], type = "l")
plot(colnames(boris)[-1],sgolayfilt(spectra_raw[100,wavenumbers <= 1900 & wavenumbers >= 700],2,11,2), type = "l")

plot(colnames(boris)[-1],boris[500,-1], type = "l")
plot(colnames(tmp),tmp[which(samples == boris[500,1]),]*-1, type = "l")

