read_pt_weather_data <- function(files){
  tmp <- readLines(files)
  df <- read.csv(text = tmp[-4], 
                 header = T,
                 skip = 2,
                 nrows = length(tmp)-9)
  df <- df[,-c(seq.int(3,ncol(df),2),ncol(df))]
  tmp <- colnames(df)
  tmp[-1] <- sapply(tmp[-1], function(string) sub("\\.","/",substring(string,regexpr("\\.\\.\\d",string)+2,last = nchar(string)-1 )))
  colnames(df) <- tmp
  return(df)
}

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

extract_from_krige <- function(data, kriged, length = 14, sum = T){
  require(reshape2)
  melted <- melt(kriged)
  colnames(melted) <- c("lon","lat","date","value","sd")
  melted[melted$value < 0, 4] = 0
  tmp_env <- sapply(c(1:nrow(data)), function(x){
    # extract date series out of dataframe
    tmp <- subset(melted, date < data$Date[x] & date > data$Date[x] - length)
    # extract by location
    tmp2 <- tmp[tmp[,2] %in% round(data[x,"Latitude"],1) & tmp[,1] %in% round(data[x, "Longitude"],1),4]
    if(length(tmp2) == 0){
      tmp2 <- tmp[as.numeric(as.character(tmp[,2])) %in% round(data[x,"Latitude"],1) & tmp[,1] %in% round(data[x, "Longitude"],1),4]
    }
    # aggregate env stat
    if(sum){
      sum(tmp2)
    }else{
      mean(tmp2)
    }
  })
}

lapsed_temp <- function(old_alt, new_alt, oldT, lapse_rate = 9.8){
  dT <- lapse_rate * -(new_alt - old_alt)
  newT <- oldT + dT
  return(newT)
}

NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))