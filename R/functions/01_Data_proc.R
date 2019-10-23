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

SG_smooth <- function(spectra, deriv){
  library(EMSC)
  res = SavitzkyGolay(spectra, deriv = deriv, poly = 3, width = 11)
  res_emsc = EMSC(res)
  colnames(res_emsc$corrected) = colnames(spectra)
  return(res_emsc$corrected)
}

mean_spectra <- function(data, ID){
  tmp = as.matrix(do.call(rbind,lapply(unique(ID), function(x) colMeans(data[ID == x,]))))
  return(tmp)
}