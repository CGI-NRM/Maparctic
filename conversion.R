library(leaflet)
library(shiny)
library(RColorBrewer)
library(readxl)


# Example file from Eriks data
location <- read_excel("Data/Examples.xlsx")

# Convert degree, second format to decimals. NB! This give the same res as google maps, but I am not sure it actually yields the correct positions
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

locs <- degreToDec(location$Lat, location$Lon)
locs$Occupied <- location$Occupied
locs$Name <- location$Name

# Create a palette that maps factor levels to colors
pal <- colorFactor(c("navy", "red"), domain = c("y", "n"))

leaflet(locs) %>% addTiles() %>%
  addCircles(
    lng = ~Lon,
    lat = ~Lat,
    radius = 6000,
    color = ~pal(Occupied),
    stroke = FALSE, fillOpacity = 0.5,
    popup = ~Name)
