extract_from_raster <- function(data, raster_folder = here("Data","Input","soil"), names = c("soil_m_0.07", "soil_m_0.21", "soil_m_0.72", "soil_m_1.89")){
  require(raster)
  files <- list.files(raster_folder, full.names = T)
  r <- lapply(files, function(x) raster(x))
  coordinates(data)<-~Longitude+Latitude
  crs(data)<-CRS("+init=EPSG:4326")
  l <- lapply(c(1:length(r)), function(x){
    tmp <- extract(r[[x]], data)
    sel <- is.na(tmp)
    d <- extract(r[[x]], data[sel,], buffer = 10000)
    d <- sapply(c(1:length(d)), function(x){mean(d[[x]], na.rm=T)})
    tmp[sel]<- d
    tmp
  })
  names(l) <- names
  soil_moist <- base::as.data.frame(l)
  soil_moist$ID <- data_org$ID
  return(soil_moist)
}