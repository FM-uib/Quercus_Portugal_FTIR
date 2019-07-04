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