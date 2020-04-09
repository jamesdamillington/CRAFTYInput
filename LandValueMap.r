#script rasterizes land price shapefile 
#(code here moved from create_CRAFTY_regionCSV2.0_JM_2018-04-27.r 2018-07-05) 

rm(list=ls())

library(raster)
library(sf)

#unzip and read shapefile
unzip(zipfile="Data/LandPrice.zip",exdir="Data/LandPrice")  #unzip
Landprice<- st_read("Data/LandPrice/Native_vegetation.shp")

#read raster files (for rasterization)
lcMap <- raster(paste0("Data/ObservedLCmaps/LandCover",year,"_PastureB_Disagg.asc"))
munis.r <- raster("Data/sim10_BRmunis_latlon_5km.asc")

yrs <- seq(from=2001,to=2017,by=1)

for(year in yrs){

  #year <- 2002
  
  #rasterise year 
  Lpr<-rasterize(Landprice, munis.r, field=paste0("X",year))
  
  #get max Lpr value for this year to use in re-scale
  Lpr.max <- cellStats(Lpr, max, na.rm=T)
  
  #re-scale to 0.8-1 for Capital
  Lpr.scaled <- Lpr
  values(Lpr.scaled)= 1-0.2*((round((Lpr.max-(values(Lpr)))/Lpr.max, digits=3)))
  #plot(Lpr.scaled)
  #where land cover is not nature, price should be 1 [not needed if calculating Nature Capital from LandValue in CRAFTY
  #Lpr.scaled[lcMap != 1] <- 1
  
  #output data
  #writeRaster(Lpr, paste0("Data/LandPrice/LandPrice",year,".asc"), format = 'ascii', overwrite=T)
  writeRaster(Lpr.scaled, paste0("Data/LandPrice/LandPrice_Capital_08_",year,".asc"), format = 'ascii', overwrite=T)
}
#remove unzipped files
unlink("Data/LandPrice", recursive = T)

