#script rasterizes land protection shapefiles 


rm(list=ls())

library(raster)
library(sf)


#unzip and read files
#unzip(zipfile="Data/LandPrice.zip",exdir="Data/LandPrice")  #unzip

unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data")  #unzip
munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")

LandProtect<- st_read("Data/landProtection/protected_areas_clip.shp")
lc2001 <- raster("Data/ObservedLCmaps/brazillc_2001_PastureB.asc")

plot(LandProtect["FID_ucsmi"])

#rasterise
Lp2001<- LandProtect["FID_ucsmi"]
Lpr<-rasterize(Lp2001, munis.r, field='FID_ucsmi')  

#set all values to 1
LP_bin <- Lpr
LP_bin[!is.na(Lpr)] <- 0
LP_bin[is.na(Lpr)] <- 1

plot(LP_bin)

#output data
writeRaster(LP_bin, "Data/ProtectionAreas.asc", format = 'ascii', overwrite=T)
