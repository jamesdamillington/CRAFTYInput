#script to convert road distances into PortAccess capital map

rm(list=ls())
library(raster)


munis.r <- raster("Data/sim10_BRmunis_latlon_5km.asc")
latlong <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
crs(munis.r) <- latlong

scaling <- -90000 #30,000 is ~max distance on the map

rd2000 <- raster("Data/PortAccessCapital/road_distance2000_2020.tif")
plot(rd2000)

rd2000.m<- resample(rd2000, munis.r, method='bilinear')
rd2000.m <- mask(x=rd2000.m, mask=munis.r)

plot(rd2000.m)

hist(values(rd2000.m))
summary(values(rd2000.m))

#rescale 0-1
values(rd2000.m)=values(rd2000.m)*(1/scaling)  
values(rd2000.m)=round(values(rd2000.m)+1,3)


hist(values(rd2000.m))

#repeat for 2005, 2010, 2017

#2005
rd2005 <- raster("Data/PortAccessCapital/road_distance2005_2020.tif")
rd2005.m<- resample(rd2005, munis.r, method='bilinear')
rd2005.m <- mask(x=rd2005.m, mask=munis.r)
values(rd2005.m)=values(rd2005.m)*(1/scaling)  
values(rd2005.m)=round(values(rd2005.m)+1,3)

hist(values(rd2005.m))
summary(values(rd2005.m))
#plot(rd2005.m)

#2010
rd2010 <- raster("Data/PortAccessCapital/road_distance2010_2020.tif")
rd2010.m<- resample(rd2010, munis.r, method='bilinear')
rd2010.m <- mask(x=rd2010.m, mask=munis.r)
values(rd2010.m)=values(rd2010.m)*(1/scaling)  
values(rd2010.m)=round(values(rd2010.m)+1,3)

hist(values(rd2010.m))
summary(values(rd2010.m))
#plot(rd2010.m)



#2017
rd2017 <- raster("Data/PortAccessCapital/road_distance2017_2020.tif")
rd2017.m<- resample(rd2017, munis.r, method='bilinear')
rd2017.m <- mask(x=rd2017.m, mask=munis.r)
values(rd2017.m)=values(rd2017.m)*(1/scaling)  
values(rd2017.m)=round(values(rd2017.m)+1,3)

hist(values(rd2017.m))
summary(values(rd2017.m))
#plot(rd2017.m)


writeRaster(rd2000.m, "Data/PortAccessCap2020_2000", "ascii", "overwrite"=T)
writeRaster(rd2005.m, "Data/PortAccessCap2020_2005", "ascii", "overwrite"=T)
writeRaster(rd2010.m, "Data/PortAccessCap2020_2010", "ascii", "overwrite"=T)
writeRaster(rd2017.m, "Data/PortAccessCap2020_2017", "ascii", "overwrite"=T)

