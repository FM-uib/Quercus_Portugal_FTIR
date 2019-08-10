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

pc_plots <- function(plot, expl_var, alpha = 1, size = 3) {
  require(ggplot2)
  require(ggsci)
  theme_set(theme_bw())
  
  plot1 <- ggplot(plot, aes(C1,C2, color = Species)) +
    geom_hline(yintercept = 0, alpha = .5) + geom_vline(xintercept = 0, alpha = .5) +
    geom_point(size = size, alpha = alpha, aes(shape = Section)) + coord_equal() +
    scale_color_npg() + stat_ellipse() +
    xlab(paste0("Component 1 (",round(expl_var[1],1), " %)")) + ylab(paste0("Component 2 (",round(expl_var[2],1), " %)")) + 
    scale_x_continuous(limits=c(-30, 20)) + scale_y_continuous(limits=c(-15, 21)) +
    scale_shape_manual(values = c(15:17)) + ggtitle("a)") +
    theme(legend.position = "none", text = element_text(size = 18),
          plot.title = element_text(margin = margin(t = -10, b = -20)))

  plot2 <- ggplot(plot,aes(C3,C4, color = Species)) +
    geom_hline(yintercept = 0, alpha = .5) + geom_vline(xintercept = 0, alpha = .5) +
    geom_point(size = size, alpha = alpha, aes(shape = Section)) + coord_equal() +
    scale_color_npg() + stat_ellipse() +
    xlab(paste0("Component 3 (",round(expl_var[3],1), " %)")) + ylab(paste0("Component 4 (",round(expl_var[4],1), " %)")) +
    scale_x_continuous(limits=c(-12, 12)) + scale_y_continuous(limits=c(-10, 10)) +
    scale_shape_manual(values = c(15:17)) + ggtitle("b)") +
    theme(text = element_text(size = 18), plot.title = element_text(margin = margin(t = -10, b = -20)))

  results <- list(plot1, plot2)
  return(results)
}