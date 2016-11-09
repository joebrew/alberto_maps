library(raster)
library(ggplot2)
library(dplyr)
library(maptools)
library(rgdal)

#### MOZAMBIQUE
# Get a shapefile for Mozambique
moz <- raster::getData("GADM", country = "MOZ", level = 3)
moz_fortified <-
  fortify(moz, region = 'NAME_3')

#### MAPUTO
maputo <- moz[moz@data$NAME_1 %in% c('Maputo', 'Maputo City'),]
# Fortify maputo (format for ggplot2)
maputo_fortified <- 
  fortify(maputo, region = 'NAME_1')

#### MANHICA
manhica <- moz[moz@data$NAME_2 == "Manhiça",]
# Fortify manhica (format for ggplot2)
manhica_fortified <- 
  fortify(manhica, region = 'NAME_3')
# Get lng and lat in manhica@data
manhica@data$lng <- coordinates(manhica)[,1]
manhica@data$lat <- coordinates(manhica)[,2]

#### magude
magude <- moz[moz@data$NAME_2 == "Magude",]
# Fortify magude (format for ggplot2)
magude_fortified <- 
  fortify(magude, region = 'NAME_3')
# Get lng and lat in magude@data
magude@data$lng <- coordinates(magude)[,1]
magude@data$lat <- coordinates(magude)[,2]

# Combine manhica and magude
combined_map <-
  rbind(magude, manhica)

# Combined fortified
combined_fortified <- rbind(magude_fortified %>% mutate(District = 'Magude'),
                  manhica_fortified %>% mutate(District = 'Manhiça'))
combined_fortified$District <- factor(combined_fortified$District,
                            levels = c('Magude',
                                       'Manhiça'))

# Get africa shapefile
africa <- readOGR('africa_shapefile', 'africa_shapefile')

pdf('maps.pdf', width = 11, height = 8.5)
par(mfrow = c(1, 3))
par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))

 # Plot 1
plot(africa, col = 'grey', border = 'white', lwd = 0.1)
plot(moz, add = TRUE, col = adjustcolor('darkorange', alpha.f = 0.7), border = NA)

# Plot 2
plot(moz, col = 'grey', border = 'white', lwd = 0.1)
plot(maputo, add = TRUE, col = adjustcolor('darkorange', alpha.f = 0.7), border = NA)

# Plot 3
plot(maputo, col = 'grey', border = 'white', lwd = 0.1)
plot(manhica, add = TRUE, col = adjustcolor('darkgreen', alpha.f = 0.7), border = NA)
plot(magude, add = TRUE, col = adjustcolor('darkred', alpha.f = 0.7), border = NA)

dev.off()