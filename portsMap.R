
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

