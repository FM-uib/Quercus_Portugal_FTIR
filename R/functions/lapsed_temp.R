lapsed_temp <- function(old_alt, new_alt, oldT, lapse_rate = 9.8){
  dT <- lapse_rate * -(new_alt - old_alt)
  newT <- oldT + dT
  return(newT)
}