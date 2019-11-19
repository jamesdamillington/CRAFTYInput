#script rasterizes land price shapefile 
#(code here moved from create_CRAFTY_regionCSV2.0_JM_2018-04-27.r 2018-07-05) 

rm(list=ls())

library(raster)
library(sf)

year <- 2016

#unzip and read files
unzip(zipfile="Data/LandPrice.zip",exdir="Data/LandPrice")  #unzip

Landprice<- st_read("Data/LandPrice/Native_vegetation.shp")
lcMap <- raster(paste0("Data/ObservedLCmaps/LandCover",year,"_PastureB_Disagg.asc"))
munis.r <- raster("Data/sim10_BRmunis_latlon_5km.asc")

#rasterise year 
Lpr<-rasterize(Landprice, munis.r, field=paste0("X",year))

#re-scale to 0-1 for Capital
Lpr.scaled <- Lpr
values(Lpr.scaled)= round((1500-(values(Lpr)))/1500, digits=3)

#where land cover is not nature, price should be 1 
Lpr.scaled[lcMap != 1] <- 1

#output data
writeRaster(Lpr, paste0("Data/LandPrice",year,".asc"), format = 'ascii', overwrite=T)
writeRaster(Lpr.scaled, paste0("Data/LandPrice",year,"_Capital_nat1.asc"), format = 'ascii', overwrite=T)

#remove unzipped files
unlink("Data/LandPrice", recursive = T)

