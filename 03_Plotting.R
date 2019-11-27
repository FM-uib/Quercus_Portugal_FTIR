library(here)

source(here("R", "functions", "03_plotting.R"))
data_mean = readRDS(file = here("Data", "Output", "data_mean.rds"))
folded_pls = readRDS(file = here("Data", "Output", "folded_pls.rds"))

# Map
figure1 = map_plot(data_mean)

ggsave(here("R","figures","figure1.png"), plot = figure1, device = "png", 
       width = 20, height = 20, units = c("cm"), dpi = 600)


figure2 = plot_mean_spectra(data_mean)
ggsave(here("R","figures","figure2.png"),width = 30, height = 20, units = "cm", plot = figure2, dpi = 600)

plt = pc_plots(folded_pls, data = data_mean)
figure3 = rbind(ggplotGrob(plt[[1]]), ggplotGrob(plt[[2]]), size = "first")
grid.newpage()
grid.draw(figure3)

ggsave("figure3.png", plot = figure3, device = "png", path = here("R", "figures"), width = 20, height = 20, units = "cm", dpi = 600)

figure4 = loadings_plot(folded_pls)
ggsave("figure4.png", plot = figure4, device = "png", path = here("R", "figures"), width = 20, height = 30, units = "cm", dpi = 600)
