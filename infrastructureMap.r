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
d2 <- raster("Data/d2Ports2000.asc")
plot(d2)

d2.m<- resample(d2, munis.r, method='bilinear')
d2.m <- mask(x=d2.m, mask=munis.r)

plot(d2.m)

hist(values(d2.m))
summary(values(d2.m))

values(d2.m)=values(d2.m)/1000
values(d2.m)=values(d2.m)*(1/-2530)
values(d2.m)=values(d2.m)+1

#3 lines above equivalent to 
#values(d2.m)=1+(values(d2.m)/-2530000)

#where does 2530000 come from? is it 2 * max possible distance in the map?
#summary(values(d2.m))
#2*1101446
#1000*2530

values(d2.m)=round(values(d2.m), digits=3)

hist(values(d2.m))
d2.m

hist(values(d2.m))
summary(values(d2.m))

#but this map is the straght-line distance to ports... we need to do better than that
plot(d2.m, zlim= c(0.4,1))


#instead I think we shoudl be using values from Daniel's maps
r1997 <- raster("Data/rail_road_distance1997.tif")
r2007 <- raster("Data/rail_road_distance2007.tif")

plot(r1997, main="1997")
plot(r2007, main="2007")

r1997.m <- resample(r1997, munis.r, method='bilinear')
r1997.m <- mask(x=r1997.m, mask=munis.r)

#when using the scaling above we get a very narrow range of values
values(r1997.m)=1+(values(r1997.m)/-2530000)
values(r1997.m)=round(values(r1997.m), digits=3)

hist(values(r1997.m))

##this is because the maximum value in these maps is smaller than in the map above
summary(r1997)
#~58,700 here vs ~1,100,000 above

#so rescale differently
r1997.m <- resample(r1997, munis.r, method='bilinear')
r1997.m <- mask(x=r1997.m, mask=munis.r, updatevalue=40000,updateNA=F) #supply mask value othewise some cells in mask are NA (use join in create_regionCSV script) 
values(r1997.m)=1+(values(r1997.m)/-100000)
plot(r1997.m)


hist(values(r1997.m))

##OR need to keep original scaling to take account of all of Brazil
#i.e. use numbers on the unmasked map
r1997.m <- resample(r1997, munis.r, method='bilinear')
summary(r1997.m)
#max value ~59,000... so maybe not a problem

values(r1997.m)=1+(values(r1997.m)/-100000)
hist(values(r1997.m))
plot(r1997.m, main="1997rescaled", zlim= c(0.4,1))

#compare after masked to study area
r1997.m <- mask(x=r1997.m, mask=munis.r)
hist(values(r1997.m))
plot(r1997.m, main="1997rescaled", zlim= c(0.4,1))
summary(r1997.m)
#masking removes values <0.7

#now check with 2007 data using same scaling as for 1997 (all Brazil)
r2007.m <- resample(r2007, munis.r, method='bilinear')
values(r2007.m)=1+(values(r2007.m)/-100000)
hist(values(r2007.m))

#all brazil
plot(r2007.m, main="2007rescaled", zlim= c(0.4,1))

#mask to study area
r2007.m <- mask(x=r2007.m, mask=munis.r) #supply mask value othewise some cells in mask are NA (use join in create_regionCSV script)  )
plot(r2007.m, main="2007rescaled", zlim= c(0.4,1))

values(r1997.m)=round(values(r1997.m), digits=3)
values(r2007.m)=round(values(r2007.m), digits=3)

hist(r1997.m)
hist(r2007.m)

writeRaster(r1997.m, "Data/infrastructureCap1997", "ascii", "overwrite"=T)
writeRaster(r2007.m, "Data/infrastructureCap2007", "ascii", "overwrite"=T)


