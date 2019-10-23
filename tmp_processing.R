library(here)

raw = read.csv(here("Data_safe","Input","Quercus_ATR_RAW.csv"), check.names = F)
spectra_raw = as.matrix(raw[,1:3318])
tmp = spectral_processing(spectra_raw)

samples = sapply(raw$Filename, str_sub, start = -13L , end = -3L)
boris = read.csv(here("Data","Input","Quercus_ATRportugal2ndDerMSC.csv"), check.names = F)

plot(colnames(boris)[-1],boris[900,-1], type = "l")
plot(colnames(tmp),tmp[which(samples == boris[900,1]),], type = "l")

spectral_processing <- function(spectra, poly = 2, width = 11, deriv = 2, scale = T){
  #' Preprocess the data for Analysis.
  #' 
  #' @description Preprocess the raw spectra by calculating the second derivative using Savitzky Golay smoothing.
  #' Spectra is also cropped to spectral region of interest 700 to 1900 cm-1.
  #' @param spectra matrix. raw spectra to process
  #' @return returns the second derivate and smoothed input spectra
  #'
  require(EMSC)
  M = SavitzkyGolay(spectra, poly = poly, width = width, deriv = deriv)
  if(scale) M = M * -1000 else M = M * -1
  colnames(M) = colnames(spectra)
  wavenumbers = as.numeric(colnames(M))
  M = M[,wavenumbers <= 1900 & wavenumbers >= 700]
  return(M)
}

c(1:10) > 3 & c(1:10) < 8
tmp = SavitzkyGolay(spectra_raw,2,11,2)
wavenumbers = as.numeric(colnames(spectra_raw))
colnames(tmp) = as.numeric(colnames(spectra_raw))
tmp = tmp[,wavenumbers <= 1900 & wavenumbers >= 700]

replicates = sapply(raw$Filename, str_sub, start = -13L , end = -6L)
samples = sapply(raw$Filename, str_sub, start = -13L , end = -3L)

tmp_emsc = EMSC(tmp, degree = 6, replicates = replicates, rep_corr = .9)

boris = read.csv(here("Data","Input","Quercus_ATRportugal2ndDerMSC.csv"), check.names = F)


plot(colnames(boris)[-1],boris[1,-1], type = "l")
plot(colnames(boris)[-1],sgolayfilt(spectra_raw[100,wavenumbers <= 1900 & wavenumbers >= 700],2,11,2), type = "l")

plot(colnames(boris)[-1],boris[500,-1], type = "l")
plot(colnames(tmp),tmp[which(samples == boris[500,1]),]*-1, type = "l")

