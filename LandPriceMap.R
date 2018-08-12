#script rasterizes land price shapefile 
#(code here moved from create_CRAFTY_regionCSV2.0_JM_2018-04-27.r 2018-07-05) 

rm(list=ls())

library(raster)
library(sf)


#unzip and read files
unzip(zipfile="Data/LandPrice.zip",exdir="Data/LandPrice")  #unzip

unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data")  #unzip
munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")

Landprice<- st_read("Data/LandPrice/Native_vegetation.shp")
lc2001 <- raster("Data/ObservedLCmaps/brazillc_2001_PastureB.asc")

#rasterise year 2001
Lp2001<- Landprice[,-(4:19)]
Lp2001<- Lp2001[,-(1:2)]
Lpr<-rasterize(Lp2001, munis.r, field='X2001')  

#re-scale to 0-1 for Capital
Lpr.scaled <- Lpr
values(Lpr.scaled)= round((1500-(values(Lpr)))/1500, digits=3)

#where land cover is not nature, price should be 1 
Lpr.scaled[lc2001 != 1] <- 1

#output data
writeRaster(Lpr, "Data/LandPrice2001.asc", format = 'ascii', overwrite=T)
writeRaster(Lpr.scaled, "Data/LandPrice2001_Capital_nat1.asc", format = 'ascii', overwrite=T)

#remove unzipped files
unlink("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")
unlink("Data/LandPrice", recursive = T)
