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