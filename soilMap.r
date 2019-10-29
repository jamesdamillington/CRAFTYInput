#script resamples original soil texture data and converts to values neeed for moistureMap.r (etc)

rm(list=ls())
library(raster)

#read munis.r as latlong
unzip(zipfile="Data/sim10_BRmunis_latlon_5km.zip",exdir="Data")  #unzip
munis.r <- raster("Data/sim10_BRmunis_latlon_5km.asc")
latlong <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
crs(munis.r) <- latlong

#note textura_wgs84.asc in zip file is 1.6GB!
unzip(zipfile="Data/textura_wgs84.zip",exdir="Data")  #unzip
vsoil<-raster("Data/textura_wgs84.asc")

#resample soil maps from ~250m resolution to 5km 
vsoil.m<- resample(vsoil, munis.r, method='ngb')  
vsoil.m<- mask(x=vsoil.m, mask=munis.r)   
writeRaster(vsoil.m, "Data/soilT_2018-05-01", "ascii", "overwrite"=T)

#classify soil texture map for CRAFTY
#classification is shown in SoilClassification.docx 
#plot(vsoil)
values(vsoil.m)[values(vsoil.m)==1] = 0
values(vsoil.m)[values(vsoil.m)==2] = 0
values(vsoil.m)[values(vsoil.m)==3] = 1
values(vsoil.m)[values(vsoil.m)==4] = 0.5
values(vsoil.m)[values(vsoil.m)==5] = 0.1

#write to file
writeRaster(vsoil.m, "Data/vsoil_2018-05-08", "ascii", "overwrite"=T) 

unlink("Data/textura_wgs84.asc")  #large file so delete! 
unlink("Data/sim10_BRmunis_latlon_5km.asc")