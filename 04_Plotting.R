library(here)
library(ggplot2)
library(rgdal)
library(maptools)
library(dplyr)
library(grid)
library(gridExtra)

source(here("R", "functions", "04_plotting.R"))
data = readRDS(file = here("Data", "Output", "data_mean.rds"))
folded_pls = readRDS(file = here("Data", "Output", "folded_pls.rds"))

# Map
figure1 = map_plot(data)

ggsave(here("R","figures","figure1.png"), plot = figure1, device = "png", 
       width = 15, height = 15, units = c("cm"), dpi = 600)


figure2 = plot_mean_spectra(data)
ggsave(here("R","figures","figure2.png"),width = 30, height = 20, units = "cm", plot = figure2, dpi = 600)

plt = pc_plots(folded_pls, data = data)
grid.newpage()
grid.draw(cbind(ggplotGrob(plt[[1]]), ggplotGrob(plt[[2]]), size = "last"))

ggsave("figure3.png", plot = figure3, device = "png", path = here("R", "figures"), width = 35, height = 10, units = "cm", dpi = 600)

figure4 = loadings_plot(folded_pls)
ggsave("figure4.png", plot = figure4, device = "png", path = here("R", "figures"), width = 20, height = 30, units = "cm", dpi = 600)
