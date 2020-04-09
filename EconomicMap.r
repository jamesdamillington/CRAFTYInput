
rm(list=ls())

library(raster)

LC <- raster("C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/CRAFTYInput/Data/ObservedLCmaps/LandCover2001_PastureB_Disagg.asc")  #land cover from LandCoverMap.r (or ClassifyDisaggregateMap.r)
plot(LC)

LC[LC == 2] <- 0.1
LC[LC == 3] <- 0.1
LC[LC == 1] <- 0.8
LC[LC == 4] <- 0.8
LC[LC == 5] <- 0.8


writeRaster(LC, "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/CRAFTYInput/Data/AgriLocations2001.asc", format = 'ascii', overwrite=T)

