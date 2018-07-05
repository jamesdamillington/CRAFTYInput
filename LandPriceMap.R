#script rasterizes land price shapefile 
#(code here moved from create_CRAFTY_regionCSV2.0_JM_2018-04-27.r 2018-07-05) 

rm(list=ls())

library(raster)
library(sf)


unzip(zipfile="Data/LandPrice.zip",exdir="Data/LandPrice")  #unzip

unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data")  #unzip
munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")

Landprice<- st_read("Data/LandPrice/Native_vegetation.shp")
Lp2001<- Landprice[,-(4:19)]
Lp2001<- Lp2001[,-(1:2)]
Lpr<-rasterize(Lp2001, munis.r, field='X2001')  

writeRaster(Lpr, "Data/LandPrice2001.asc", format = 'ascii', overwrite=T)


unlink("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")
unlink("Data/LandPrice", recursive = T)
