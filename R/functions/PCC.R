PCC_variability = function(spectra, id){
  #' Calculates the mean Pearson Correlation coefficient for grouped spectra.
  #'
  #' @description This function is a wrapper for pearson.dist() from the hyperSpec package
  #' and calculates the mean Pearson Correlation coefficient for multiple spectra belonging
  #' to specified groups, which can be e.g. replicates. 
  #' 
  #' @param spectra matrix. Matrix of spectra. Samples as rows, wavenumbers as columns.
  #' @param id vector. Character or numerical vector indicating group identity.
  if(!dim(spectra)[1]==length(id)){
    stop("Number of spectra and identifier not the same length.")
  }
  require(hyperSpec)
  unique_id = unique(id)
  PCC_dist = as.matrix(pearson.dist(spectra))
  results = data.frame(ID = unique(id),
                       n = c(1:length(unique(id))))
  results$mean_PCC = sapply(unique_id, function(x){
    bool = id %in% x
    tmp = as.vector(PCC_dist[bool,bool])
    return(mean(unique(tmp)[-1]))
  })
  results$sd_PCC = sapply(unique_id, function(x){
    bool = id %in% x
    tmp = as.vector(PCC_dist[bool,bool])
    return(sd(unique(tmp)[-1]))
  })
  results$n = sapply(unique_id, function(x){
    bool = id %in% x
    tmp = as.vector(PCC_dist[bool,bool])
    return(length(unique(tmp)[-1]))
  })  
  return(results)
}
