spectral_processing <- function(spectra, poly = 2, width = 11, deriv = 2, scale = T){
  #' Preprocess the data for Analysis.
  #' 
  #' @description Preprocess the raw spectra by calculating the second derivative using Savitzky Golay smoothing.
  #' Spectra is also cropped to spectral region of interest 700 to 1900 cm-1.
  #' @param spectra matrix. raw spectra to process
  #' @return returns the second derivate and smoothed input spectra
  #'
  require(EMSC)
  sg_spectra = SavitzkyGolay(spectra, poly = poly, width = width, deriv = deriv)
  
  wavenumbers = as.numeric(colnames(spectra))  
  sg_spectra_red = sg_spectra[,wavenumbers <= 1900 & wavenumbers >= 700]
  emsc_spectra = EMSC(sg_spectra_red)
  M = emsc_spectra$corrected
  colnames(M) = colnames(spectra[,wavenumbers <= 1900 & wavenumbers >= 700])
  #if(scale) M = M * -1000 else M = M * -1
  return(M)
}

mean_spectra <- function(data, ID){
  tmp = as.matrix(do.call(rbind,lapply(unique(ID), function(x) colMeans(data[ID == x,]))))
  return(tmp)
}