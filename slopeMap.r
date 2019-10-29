#script resamples original slope data and converts to values neeed for moistureMap.r (etc)

rm(list=ls())
library(raster)

#read munis.r as latlong
unzip(zipfile="Data/sim10_BRmunis_latlon_5km.zip",exdir="Data")  #unzip
munis.r <- raster("Data/sim10_BRmunis_latlon_5km.asc") #spatial resolution is 5000m
latlong <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
crs(munis.r) <- latlong

#note decliv_class.asc in zip file is 1.8GB!
unzip(zipfile="Data/decliv_class.zip",exdir="Data")  #unzip
vslope<-raster("Data/decliv_class.asc")  #decliv has spatial resolution 250m

vslope.m10 <- aggregate(vslope, fact=20, fun=mean)  #fact=20 as 5000m / 250m = 20
vslope.m10 <- resample(vslope.m10, munis.r, method='ngb')  
vslope.m10 <- mask(x=vslope.m10, mask=munis.r)  

plot(vslope.m10)

vslope.sd <- aggregate(vslope, fact=20, fun=sd) 


#use reclassify() method below https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/reclassify
#proof of concept
#a <- c(0,1)
#a <- rbind(a,(c(1,2)))
#r <- raster(ncol=10, nrow=10)
#values(r) <- rbinom(ncell(r), size = 1, prob = 0.5)
#plot(r)
#r <- reclassify(r, a)
#plot(r)

a <- c(1,0)
a <- rbind(a,(c(2,1)))
a <- rbind(a,(c(3,2)))
a <- rbind(a,(c(4,3)))
a <- rbind(a,(c(5,4)))
a <- rbind(a,(c(6,4)))

reclassify(vslope.m, a)

writeRaster(vslope.m, "Data/vslope_unclassified_2019-06-07", "ascii", "overwrite"=T)


unlink("Data/decliv_class.asc")  #large file so delete! 
unlink("Data/sim10_BRmunis_latlon_5km.asc")
