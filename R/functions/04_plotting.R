require(directlabels)

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

loadings_plot <- function(folds, comps = 4){
  library(reshape2)
  library(ggplot2)
  library(dplyr)
  Expl_Var = colMeans(t(sapply(c(1:length(folds)), function(x) explvar(folds[[x]]$'fitted model'))))
  
          
  tmp = lapply(c(1:length(folds)), function(x) folds[[x]]$`fitted model`$loadings[,c(1:comps)])
  tmp = (do.call(rbind, tmp))
  tmp2 = as.data.frame(tmp)
  colnames(tmp2) = sapply(1:comps, function(x) paste0("Component ",x))
  tmp2$Wavenumbers = as.numeric(rownames(tmp))
  
  x_load = tmp2 %>%
    group_by(Wavenumbers) %>%
    summarise_all(mean)
  
  peaks = data.frame(peaks_wn = as.numeric (c("1745", "1462", "721",
                                              "1651", "1641", "1551", "1535",
                                              "1107", "1076", "1055", "1028", "995",
                                              "1605", "1516", "1171", "852", "833", "816")),
                     peaks_c = as.character(c(rep("L",3), rep("P", 4), rep("C",5), rep("S",6))))
  peaks = peaks[order(peaks$peaks_wn, decreasing = F),]
  
  x_load = x_load[round(x_load$Wavenumbers) %in% peaks$peaks_wn, ]
  x_load$ID = paste(peaks$peaks_c,peaks$peaks_wn)
  #x_load$col = as.factor(peaks$peaks_c)
  x_load = melt(x_load[,-1], id.vars = "ID")
  x_load$variable = factor(x_load$variable, labels = sapply(1:comps, function(x) paste0("Component ",x," (Explained Variance: ",round(Expl_Var[x]), "%)")))
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

pc_plots <- function(folds, data, comps = 4, alpha = 1, size = 3) {
  require(ggplot2)
  require(ggsci)
  theme_set(theme_bw())
  expl_var = colMeans(t(sapply(c(1:length(folds)), function(x) explvar(folds[[x]]$'fitted model'))))
  
  tmp = lapply(c(1:length(folds)), function(x) folds[[x]]$`fitted model`$scores[,1:comps])
  tmp = (do.call(rbind, tmp))
  tmp2 = as.data.frame(tmp)
  colnames(tmp2) = sapply(1:comps, function(x) paste0("C",x))
  tmp2$rID = rownames(tmp)
  
  library(dplyr)
  plot_data = tmp2 %>%
    group_by(rID) %>%
    summarise_all(mean)
  
  plot_data$Species = data[plot_data$rID,"Sub_Spec"]
  plot_data$Section = data[plot_data$rID,"Section"]
  
  plot1 <- ggplot(plot_data, aes(C1,C2, color = Species)) +
    geom_hline(yintercept = 0, alpha = .5) + geom_vline(xintercept = 0, alpha = .5) +
    geom_point(size = size, alpha = alpha, aes(shape = Section)) + coord_equal() +
    scale_color_npg() + #stat_ellipse() +
    xlab(paste0("Component 1 (",round(expl_var[1],1), " %)")) + ylab(paste0("Component 2 (",round(expl_var[2],1), " %)")) + 
    scale_x_continuous(limits=c(-30, 20)) + scale_y_continuous(limits=c(-15, 21)) +
    scale_shape_manual(values = c(15:17)) + ggtitle("a)") +
    theme(legend.position = "none", text = element_text(size = 18),
          plot.title = element_text(margin = margin(t = -10, b = -20)))

  plot2 <- ggplot(plot_data,aes(C3,C4, color = Species)) +
    geom_hline(yintercept = 0, alpha = .5) + geom_vline(xintercept = 0, alpha = .5) +
    geom_point(size = size, alpha = alpha, aes(shape = Section)) + coord_equal() +
    scale_color_npg(labels = c("Q. faginea","Q. robur","Q. r. ssp. estremadurensis","Q. coccifera","Q. rotundifolia","Q. suber"), guide = guide_legend(label.theme = element_text(angle = 0, face = "italic"))) + #stat_ellipse() +
    xlab(paste0("Component 3 (",round(expl_var[3],1), " %)")) + ylab(paste0("Component 4 (",round(expl_var[4],1), " %)")) +
    scale_x_continuous(limits=c(-12, 12)) + scale_y_continuous(limits=c(-10, 10)) +
    scale_shape_manual(values = c(15:17)) + ggtitle("b)") +
    theme(text = element_text(size = 18), plot.title = element_text(margin = margin(t = -10, b = -20)))

  results <- list(plot1, plot2)
  return(results)
}

plot_mean_spectra<-function(data, sel = "Sub_Spec", sp = "FTIR"){
  require(ggplot2)
  require(ggsci)
  theme_set(theme_bw())
  
  spec_data <- as.data.frame(unclass(data[,sp]))
  spec_data$ID <- data$ID
  spec_data$Sub_Spec <- data[, sel]
  levels(spec_data$Sub_Spec) = c("Q. faginea","Q. robur","Q. r. ssp. estremadurensis","Q. coccifera","Q. rotundifolia","Q. suber")
  spec_data$Section <- data$Section
  spec_data <- melt(spec_data, id.vars = c("ID","Sub_Spec", "Section"))
  colnames(spec_data) <- c("ID",sel,"Section", "Wavelength", "Absorbance")
  spec_data$Wavelength<-as.numeric(as.character(spec_data$Wavelength))
  spec_data[,sel] <- as.factor(spec_data[,sel])
  plot_data <- spec_data %>%
    group_by(Section,Sub_Spec, Wavelength) %>%
    summarize(Absorbance = mean(Absorbance))
  plot_data$Absorbance = plot_data$Absorbance + sort(rep(seq(0,by = .025,length.out = 6),624))
  ldngs <- data.frame( Wavelength = c(1745, 1462, 721,
                                      1655, 1641, 1551, 1535,
                                      1101, 1076, 1050, 1028, 985,
                                      1605, 1516, 1168, 852, 833, 816),
                       Compound = c(rep("L",3), rep("P", 4), rep("C",5), rep("S",6)))
  
  g1 <- ggplot(data = plot_data, aes(Wavelength, Absorbance, color = Sub_Spec)) +
    geom_line(size = 1) + theme_bw() + scale_x_reverse(breaks = scales::pretty_breaks(n=10), limits = c(1900,420)) + 
    theme(axis.text.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_dl(aes(label = Sub_Spec), method = list(dl.combine("last.points"), cex = 0.8)) +
    scale_color_npg(labels = c("Q. faginea","Q. robur","Q. r. ssp. estremadurensis","Q. coccifera","Q. rotundifolia","Q. suber"), guide = "none") + 
    labs(x = bquote('Wavenumbers in'~cm^-1)) +
    geom_vline(data = ldngs, aes(xintercept = Wavelength), size = 2, alpha = .1) + 
    geom_text(data = ldngs, aes(x = Wavelength , y = 0.01, label= Compound), inherit.aes = F) 
  
  return(g1)
}

map_plot = function(data){
  Portugal.df <- kml_to_df(here("Data", "Input", "map", "Iberian_Penin.shp"))
  levels(data$Sub_Spec) = c("Q. faginea","Q. robur","Q. r. ssp. estremadurensis","Q. coccifera","Q. rotundifolia","Q. suber")
  ### Points of trees
  trees <- data[,c("ID", "Section", "Sub_Spec", "Group", "Location", "Latitude", "Longitude")]
  trees$Sub_Spec <- factor(trees$Sub_Spec)
  
  trees <- trees %>%
    group_by(Group) %>%
    mutate(mean.lat = mean(Latitude), mean.long = mean(Longitude)) %>%
    group_by(Group, Sub_Spec) %>%
    mutate(n = n_distinct(ID))
  
  trees <- lat_lon_transform(x = trees, variable = "mean.lat", factor = "Sub_Spec", 
                             offset = .15 * c(0,1,-1,-1,1,0))
  trees <- lat_lon_transform(x = trees, variable = "mean.long", factor = "Sub_Spec", 
                             offset = .15 * c(.5,.5,-.5,.5,-.5,-.5))
  
  trees <- lat_lon_transform(x = trees, variable = "new.mean.lat", factor = "Group", 
                             offset = .05 * c(0,-.5,0,0,0,0,0,0,-.5,0,0,0,0,0,1.5))
  trees <- lat_lon_transform(x = trees, variable = "new.mean.long", factor = "Group", 
                             offset = .05 * c(0,.5,0,0,0,0,0,0,-1.2,0,0,0,0,0,.5))
  
  ### Map of Samples by location
  library(RStoolbox)
  
  elev <- readRDS(here("Data", "Output", "elevation_raster.rds"))
  elev@file@name = here("Data", "Input", "environmental variables", "elevation", "PRT1_alt.grd")
  
  figure1 <- ggplot() +
    ggR(elev, geom_raster = TRUE, alpha = .25, ggLayer = T) + 
    scale_fill_gradientn(colours = gray.colors(100, alpha = .4), name = "elevation", na.value = "white", guide = F) +
    geom_path(data = Portugal.df, aes(x=long, y=lat), color = "black") + 
    xlim(-10,-6.3) + ylim(36.8, 42.1) +
    coord_equal(ratio = 1) +
    theme_bw() +
    geom_point(data = trees, aes(x = new.new.mean.long, y = new.new.mean.lat, 
                                 color = Sub_Spec, shape = Section, size = factor(n))) +
    scale_shape_manual(values = c(15:17)) +
    scale_size_manual(values = c(rep(2,3),rep(3,3),rep(4,8)),breaks = c(3,5,7), labels = c("< 3","4 - 6","> 7")) +
    scale_color_npg(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic"))) +
    labs(x = "Longitude", y = "Latitude", size = "No. of Trees", shape = "Quercus Section", color = "Species")
  return(figure1)
}