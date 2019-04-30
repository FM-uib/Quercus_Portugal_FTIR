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