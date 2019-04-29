krige_env <- function(data, stations, sel = c("DATA", "Date_num"), cell_size = .1, vario = vgm(1, "Sph", 1), hcPort = T){
  require(dplyr)
  require(reshape2)
  require(gstat)
  rnd <- function(x, dec, floor){
    if(floor){
      tmp <- floor(x * (1/dec)) / (1/dec)
    }else{
      tmp <- ceiling(x * (1/dec)) / (1/dec)
    }
    return(tmp)
  }
  grid <- melt(dplyr::select(data, -sel))
  grid$variable <- as.character(grid$variable)
  grid$x <- stations[grid$variable, "Longitude"]
  grid$y <- stations[grid$variable, "Latitude"]
  grid$z <- rep(data$Date_num, length(unique(grid$variable)))
  coordinates(grid) = ~x+y+z
  if(hcPort){
    grid_pred <- expand.grid(x = seq(-9.5, -6.8, cell_size),
                             y = seq(37.6, 42.0, cell_size),
                             z = c(min(unique(grid@coords[,"z"])):max(unique(grid@coords[,"z"]))))
    
  }else{
    grid_pred <- expand.grid(x = seq(rnd(min(grid$x),cell_size,T),rnd(max(grid$x),cell_size,F),cell_size),
                             y = seq(rnd(min(grid$y),cell_size,T),rnd(max(grid$y),cell_size,F),cell_size),
                             z = c(min(unique(grid@coords[,"z"])):max(unique(grid@coords[,"z"]))))
  }
  gridded(grid_pred) <- ~x+y+z
  prec_krig <- gstat::krige(formula = value ~ 1, grid, grid_pred, model = vario)
  return(prec_krig)
}