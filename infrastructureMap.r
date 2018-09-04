#old code from create_regionCSV
#JM edited next section of code 2018-04-30. SoyPorts2015 is missing shx file
# Ports<-readOGR(dsn = dpath, layer = "SoyPorts2000")
# x <- coordinates(Ports)[,1]
# y<- coordinates(Ports)[,2]
# xy<-cbind(x,y)
# rPorts<-rasterize(xy, munis.r)  #JM edit munis.r
# writeRaster(rPorts, "rPorts2000_2018-04-30", "ascii", "overwrite"=T)
# rPorts<-raster("rPorts2000_2018-04-30.asc")
# dd<-distance(rPorts)
# writeRaster(dd, "d2Ports2000_2018-04-30", "ascii", "overwrite"=T)
######################

#why reading tif? given JM re-made asc files (from 2000) use asc instead
#dPorts<-raster('road_distance2000.tif')

#2018-07-06
#trying to work out where the numbers in regionCSV come from

rm(list=ls())
library(raster)


munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")
latlong <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
crs(munis.r) <- latlong


##the next few lines produce values that are in the current region.csv
d2 <- raster("Data/PortAccessCapital/d2Ports2000.asc")
plot(d2)

rd2000 <- raster("Data/PortAccessCapital/road_distance2000.tif")
plot(rd2000)

rd2000.m<- resample(rd2000, munis.r, method='bilinear')
rd2000.m <- mask(x=rd2000.m, mask=munis.r)

plot(rd2000.m)

hist(values(rd2000.m))
summary(values(rd2000.m))

#rescale 0-1
values(rd2000.m)=values(rd2000.m)*(1/-60000)  #60,000 is ~2x max distance on the map
values(rd2000.m)=values(rd2000.m)+1


#2 lines above equivalent to 
#values(rd2000.m)=1+(values(rd2000.m)/-60000)

values(rd2000.m)=round(values(rd2000.m), digits=3)

hist(values(rd2000.m))

#repeat for 2005, 2010, 2017

#2005
rd2005 <- raster("Data/infrastructureCapital/road_distance2005.tif")
rd2005.m<- resample(rd2005, munis.r, method='bilinear')
rd2005.m <- mask(x=rd2005.m, mask=munis.r)
values(rd2005.m)=values(rd2005.m)*(1/-60000)  #60,000 is ~2x max distance on the map
values(rd2005.m)=values(rd2005.m)+1

hist(values(rd2005.m))
summary(values(rd2005.m))
plot(rd2005.m)

#2010
rd2010 <- raster("Data/infrastructureCapital/road_distance2010.tif")
rd2010.m<- resample(rd2010, munis.r, method='bilinear')
rd2010.m <- mask(x=rd2010.m, mask=munis.r)
values(rd2010.m)=values(rd2010.m)*(1/-60000)  #60,000 is ~2x max distance on the map
values(rd2010.m)=round(values(rd2010.m)+1,3)

hist(values(rd2010.m))
summary(values(rd2010.m))
plot(rd2010.m)



#2017
rd2017 <- raster("Data/infrastructureCapital/road_distance2017.tif")
rd2017.m<- resample(rd2017, munis.r, method='bilinear')
rd2017.m <- mask(x=rd2017.m, mask=munis.r)
values(rd2017.m)=values(rd2017.m)*(1/-60000)  #60,000 is ~2x max distance on the map
values(rd2017.m)=values(rd2017.m)+1

hist(values(rd2017.m))
summary(values(rd2017.m))
plot(rd2017.m)


writeRaster(rd2000.m, "Data/PortAccessCap2000", "ascii", "overwrite"=T)
writeRaster(rd2005.m, "Data/PortAccessCap2005", "ascii", "overwrite"=T)
writeRaster(rd2010.m, "Data/PortAccessCap2010", "ascii", "overwrite"=T)
writeRaster(rd2017.m, "Data/PortAccessCap2017", "ascii", "overwrite"=T)

######
#Old Code
# #instead I think we shoudl be using values from Daniel's maps
# r1997 <- raster("Data/rail_road_distance1997.tif")
# r2007 <- raster("Data/rail_road_distance2007.tif")
# 
# plot(r1997, main="1997")
# plot(r2007, main="2007")
# 
# r1997.m <- resample(r1997, munis.r, method='bilinear')
# r1997.m <- mask(x=r1997.m, mask=munis.r)
# 
# #when using the scaling above we get a very narrow range of values
# values(r1997.m)=1+(values(r1997.m)/-2530000)
# values(r1997.m)=round(values(r1997.m), digits=3)
# 
# hist(values(r1997.m))
# 
# ##this is because the maximum value in these maps is smaller than in the map above
# summary(r1997)
# #~58,700 here vs ~1,100,000 above
# 
# #so rescale differently
# r1997.m <- resample(r1997, munis.r, method='bilinear')
# r1997.m <- mask(x=r1997.m, mask=munis.r, updatevalue=40000,updateNA=F) #supply mask value othewise some cells in mask are NA (use join in create_regionCSV script) 
# values(r1997.m)=1+(values(r1997.m)/-100000)
# plot(r1997.m)
# 
# 
# hist(values(r1997.m))
# 
# ##OR need to keep original scaling to take account of all of Brazil
# #i.e. use numbers on the unmasked map
# r1997.m <- resample(r1997, munis.r, method='bilinear')
# summary(r1997.m)
# #max value ~59,000... so maybe not a problem
# 
# values(r1997.m)=1+(values(r1997.m)/-100000)
# hist(values(r1997.m))
# plot(r1997.m, main="1997rescaled", zlim= c(0.4,1))
# 
# #compare after masked to study area
# r1997.m <- mask(x=r1997.m, mask=munis.r)
# hist(values(r1997.m))
# plot(r1997.m, main="1997rescaled", zlim= c(0.4,1))
# summary(r1997.m)
# #masking removes values <0.7
# 
# #now check with 2007 data using same scaling as for 1997 (all Brazil)
# r2007.m <- resample(r2007, munis.r, method='bilinear')
# values(r2007.m)=1+(values(r2007.m)/-100000)
# hist(values(r2007.m))
# 
# #all brazil
# plot(r2007.m, main="2007rescaled", zlim= c(0.4,1))
# 
# #mask to study area
# r2007.m <- mask(x=r2007.m, mask=munis.r) #supply mask value othewise some cells in mask are NA (use join in create_regionCSV script)  )
# plot(r2007.m, main="2007rescaled", zlim= c(0.4,1))
# 
# values(r1997.m)=round(values(r1997.m), digits=3)
# values(r2007.m)=round(values(r2007.m), digits=3)
# 
# hist(r1997.m)
# hist(r2007.m)
# 
# writeRaster(r1997.m, "Data/infrastructureCap1997", "ascii", "overwrite"=T)
# writeRaster(r2007.m, "Data/infrastructureCap2007", "ascii", "overwrite"=T)


