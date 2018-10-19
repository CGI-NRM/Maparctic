degreToDec <- function(Lat, Lon) {
  temp <- do.call(rbind, strsplit(Lat, split = '"'))
  temp2 <- do.call(rbind, strsplit(temp[,1], split = "['°]"))
  dec <- matrix(as.numeric(temp2), nrow = 3, byrow = TRUE)
  dec <- dec/(c(1,60,3600))
  temp <- do.call(rbind, strsplit(Lon, split = '"'))
  temp2 <- do.call(rbind, strsplit(temp[,1], split = "['°]"))
  dec2 <- matrix(as.numeric(temp2), nrow = 3, byrow = TRUE)
  dec2 <- dec2/(c(1,60,3600))
  
  data.frame(cbind(Lat = rowSums(t(dec)), Lon = rowSums(t(dec2))))
  
}
