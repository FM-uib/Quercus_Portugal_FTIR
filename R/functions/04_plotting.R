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

loadings_plot <- function(pls_object, sel = c(1:4)){
  library(reshape2)
  library(ggplot2)
  
  peaks = data.frame(peaks_wn = as.numeric (c("1745", "1462", "721",
                                              "1651", "1641", "1551", "1535",
                                              "1107", "1076", "1055", "1028", "995",
                                              "1605", "1516", "1171", "852", "833", "816")),
                     peaks_c = as.character(c(rep("L",3), rep("P", 4), rep("C",5), rep("S",6))))
  peaks = peaks[order(peaks$peaks_wn, decreasing = T),]
  x_load = as.data.frame(pls_object$loadings[,])
  colnames(x_load) = sapply(c(1:ncol(x_load)), function(x) paste0("Component ",x))
  x_load = x_load[,sel]
  x_load = x_load[round(as.numeric(rownames(x_load))) %in% peaks$peaks_wn, ]
  x_load$ID = paste(peaks$peaks_c,peaks$peaks_wn)
  #x_load$col = as.factor(peaks$peaks_c)
  x_load = melt(x_load, id.vars = "ID")
  x_load_gg = ggplot(x_load, aes(ID, value, fill = rep(peaks$peaks_c,4))) + geom_col() + 
    facet_wrap(~variable, ncol = 1) +
    labs(x = bquote('Wavenumbers in'~cm^-1), y = "Loadings") +
    theme_bw() + theme(text = element_text(size = 18),
                       axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.position = "top",
                       legend.direction = "horizontal") +
    scale_fill_discrete(name = "Compounds", labels = c("Carbohydrates", "Lipids", "Protein", "Sporopollenin"))
  return(x_load_gg)
}