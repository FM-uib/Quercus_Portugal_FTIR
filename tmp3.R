data = readRDS(file = here("Data", "Input", "data_meaned.rds"))
source(here("R","functions","extract_from_raster.R"))

soil_moist <- extract_from_raster(data)
saveRDS(soil_moist, here("Data","Output","soil_moisture.rds"))
# raster with ca 10 km grid size
# m^3^ water / m^3^ volume 
# https://confluence.ecmwf.int/pages/viewpage.action?pageId=56660259




data$soil_moist <- extract(r[[1]], data[is.na(data$soil_moist),], buffer = 100)
sum(is.na(data$soil_moist))
sum(is.na(extract(r[[1]], data[is.na(data$soil_moist),], buffer = 100)))

tmp<-cbind(res3D@coords,res3D@data)
tmp$ID <- rep(seq(1,400,1),50)

tmp %>%
  group_by(ID) %>%
  summarise(mean = mean(var1.pred), x = mean(x))
