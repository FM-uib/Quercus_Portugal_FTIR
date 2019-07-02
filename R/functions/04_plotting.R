kml_to_df <- function(file){
  require(rgdal)
  require(ggplot2)
  ogr.file <- readOGR(file)
  ogr.file@data$id <- rownames(ogr.file@data)
  polygon <- fortify(ogr.file, region = "id")
  df <- merge(polygon, ogr.file@data, by = "id")
  return(df)
}

lat_lon_transform <- function(x, variable, factor, offset){
  names(offset) <- levels(x[,factor])
  new <- as.vector(x[,variable] + sapply(x[,factor], function(x) offset[x]))
  x$new <- new[,variable]
  names(x)[names(x) == "new"] <- paste("new", variable, sep = ".")
  return(x)
}