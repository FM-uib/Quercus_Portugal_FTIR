SG_smooth <- function(spectra, deriv){
  library(EMSC)
  res = SavitzkyGolay(spectra, deriv = deriv)
  colnames(res) = colnames(spectra)
  return(res)
}

mean_spectra <- function(data, ID){
  tmp = as.matrix(do.call(rbind,lapply(unique(ID), function(x) colMeans(data[ID == x,]))))
  return(tmp)
}