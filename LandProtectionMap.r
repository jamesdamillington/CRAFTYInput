#script rasterizes land protection shapefiles 


rm(list=ls())

library(raster)
library(sf)


#unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data")  #unzip if needed
munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc") #for rasterization

#LandProtect is for all services
LandProtect<- st_read("Data/landProtection/protected_areas_clip.shp")
Lp2001<- LandProtect["FID_ucsmi"]
Lpr<-rasterize(Lp2001, munis.r, field='FID_ucsmi')  

#Amazon is for Soy and Pasture 
Amazon <- st_read("Data/landProtection/Amazon_area.shp")
Amazon <- Amazon['NOME']
Amzn <- rasterize(Amazon, munis.r, field='NOME')  

#Land Protection
#set all values to 1 (not protected) or 0 (protected)
LP_bin <- Lpr
LP_bin[!is.na(Lpr)] <- 0
LP_bin[is.na(Lpr)] <- 1

#LandProtect currently is the only restriction on OAgri and Maize
#output data
writeRaster(LP_bin, "Data/All_ProtectionMap.asc", format = 'ascii', overwrite=T)

#lc 2006 needed for Soy Moratorium
lc2006 <- raster("Data/ObservedLCmaps/brazillc_2006_PastureB.asc")

#where 2006 nature AND Amazon Biome, Soy should be prevented
Soy_bin <- Amzn
Soy_bin[lc2006 != 1] <- NA

#then combine with 'standard' land protection
Soy_bin[LP_bin == 0] <- 1

#reverse and set NAs to 1
Soy_bin[Soy_bin == 1] <- 0 #reverse so protected is 0
Soy_bin[is.na(Soy_bin)] <- 1  #set NAs to 1 (unprotected)

#output data
writeRaster(Soy_bin, "Data/Soy_ProtectionMap_2006.asc", format = 'ascii', overwrite=T)

#lc 2008 needed for Beef Moratorium
lc2009 <- raster("Data/ObservedLCmaps/brazillc_2009_PastureB.asc")

#where 2008 nature AND Amazon Biome, Pature should be prevented
Pas_bin <- Amzn
Pas_bin[lc2009 != 1] <- NA

#then combine with 'standard' land protection
Pas_bin[LP_bin == 0] <- 1

#reverse and set NAs to 1
Pas_bin[Pas_bin == 1] <- 0 #reverse so protected is 0
Pas_bin[is.na(Pas_bin)] <- 1  #set NAs to 1 (unprotected)

#output data
writeRaster(Pas_bin, "Data/Pasture_ProtectionMap_2009.asc", format = 'ascii', overwrite=T)

