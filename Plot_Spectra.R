library(here)
library(ggplot2)
library(reshape2)
data = readRDS(file = here("Data", "Input", "data_meaned.rds"))



ggplot_spectra<-function(data, sel = "Sub_Spec", sp = "FTIR"){
  require(ggplot2)
  require(ggsci)
  theme_set(theme_bw())
  
  spec_data <- as.data.frame(unclass(data[,sp]))
  spec_data$ID <- data$ID
  spec_data$Sub_Spec <- data[, sel]
  spec_data$Section <- data$Section
  spec_data <- melt(spec_data, id.vars = c("ID","Sub_Spec", "Section"))
  colnames(spec_data) <- c("ID",sel,"Section", "Wavelength", "Absorbance")
  spec_data$Wavelength<-as.numeric(as.character(spec_data$Wavelength))
  spec_data[,sel] <- as.factor(spec_data[,sel])
  plot_data <- spec_data %>%
    group_by(Section,Sub_Spec, Wavelength) %>%
    summarize(Absorbance = mean(Absorbance))
  plot_data$Absorbance = plot_data$Absorbance + sort(rep(seq(0,.1,by = .02),624))
  ldngs <- data.frame( Wavelength = c(1745, 1462, 721,
                                      1655, 1641, 1551, 1535,
                                      1101, 1076, 1050, 1028, 985,
                                      1605, 1516, 1168, 852, 833, 816),
                       Compound = c(rep("L",3), rep("P", 4), rep("C",5), rep("S",6)))
  
  g1 <- ggplot(data = plot_data, aes(Wavelength, Absorbance, color = Sub_Spec)) +
    geom_line(size = 1) + theme_bw() + scale_x_reverse(breaks = scales::pretty_breaks(n=10)) + 
    theme(axis.title.x = element_blank(),axis.text.x=element_blank(), legend.position = "top") +
    scale_color_npg(labels = c("Q. faginea","Q. robur","Q. r. ssp. estremadurensis","Q. coccifera","Q. rotundifolia","Q. suber"), guide = guide_legend(title = "Species",nrow = 1,label.theme = element_text(angle = 0, face = "italic"), override.aes = list(size = 4))) +
    geom_vline(data = ldngs, aes(xintercept = Wavelength), size = 2, alpha = .1) + geom_text(data = ldngs, aes(x = Wavelength , y = 0.01, label= Compound), inherit.aes = F)

  g2 <- ggplot(data = plot_data, aes(Wavelength, Absorbance, color = Sub_Spec)) +
    geom_line(size = 1) + theme_bw() + scale_x_reverse(breaks = scales::pretty_breaks(n=10)) +
    scale_color_npg() + 
    facet_wrap(~Section, ncol = 1) + guides(color = "none") +
    labs(x = bquote('Wavenumbers in'~cm^-1)) +
    geom_vline(data = ldngs, aes(xintercept = Wavelength), size = 2, alpha = .1) + 
    geom_text(data = ldngs, aes(x = Wavelength , y = 0.01, label= Compound), inherit.aes = F)

  gg <- grid.arrange(g1,g2, heights = 1:2)
  
  return(gg)
}

y <- x %>%
  group_by(Section,Sub_Spec, Wavelength) %>%
  summarize(Absorbance = mean(Absorbance))

ldngs <- data.frame( Wavelength = c(1745, 1462, 721,
                       1655, 1641, 1551, 1535,
                       1101, 1076, 1050, 1028, 985,
                       1605, 1516, 1168, 852, 833, 816),
                     Compound = c(rep("L",3), rep("P", 4), rep("C",5), rep("S",6)))


ggplot(data = x, aes(Wavelength, Absorbance, color = Sub_Spec))+
  geom_line(aes(group = ID)) + 
  facet_wrap(~Section, ncol = 1)

g1 <- ggplot(data = y, aes(Wavelength, Absorbance, color = Sub_Spec)) +
  geom_line() + theme_bw() + scale_x_reverse(breaks = scales::pretty_breaks(n=10)) + 
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(), legend.position = "top") +
  guides(color=guide_legend(title="Species", nrow = 1)) + 
  geom_vline(data = ldngs, aes(xintercept = Wavelength), size = 2, alpha = .1) + geom_text(data = ldngs, aes(x = Wavelength , y = 0.01, label= Compound), inherit.aes = F)

  #geom_vline(data = ldngs, aes(xintercept = Wavelength, color = Compound))
g2 <- ggplot(data = y, aes(Wavelength, Absorbance, color = Sub_Spec)) +
  geom_line() + theme_bw() + scale_x_reverse(breaks = scales::pretty_breaks(n=10)) +
  facet_wrap(~Section, ncol = 1) + guides(color = "none") + 
  geom_vline(data = ldngs, aes(xintercept = Wavelength), size = 2, alpha = .1) + geom_text(data = ldngs, aes(x = Wavelength , y = 0.01, label= Compound), inherit.aes = F)

  #geom_vline(data = ldngs, aes(xintercept = Wavelength, color = Compound))
grid.arrange(g1,g2, heights = 1:2)




g3 <- ggplot()+geom_vline(data = ldngs, aes(xintercept = Wavelength, color = Compound), size = 2, alpha = .2)

geom_vline(xintercept = ldngs$Wavelength, color = c(rep("blue",3), rep("green", 4), rep("red",5), rep("black",6)), size = 2, alpha = .1)




