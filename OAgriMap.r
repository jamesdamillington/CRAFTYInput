#create map of Other Agriculture Captial
#currently this is based solely on slope (script converts to values needed)

rm(list=ls())
library(raster)


#note decliv_class.asc in zip file is 1.8GB!
#read munis.r as latlong
#unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data")  #unzip
munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")
latlong <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
crs(munis.r) <- latlong

#note decliv_class.asc in zip file is 1.8GB!
unzip(zipfile="Data/decliv_class.zip",exdir="Data")  #unzip
vslope<-raster("Data/decliv_class.asc")


vslope.m<- resample(vslope, munis.r, method='ngb')  #JM edited munis.r
vslope.m<- mask(x=vslope.m, mask=munis.r)  #JM edited munis.r

#	0 (1) becomes 0.2 – (0 – 3% No limitation)
#	1 (2) becomes 0.4 – (3 – 8% Slight limitation )
#	2 (3) becomes 0.6 – (8 – 13% Moderate limitation)
#	3 (4) becomes 0.8 – (13 – 20% Strong limitation)
#	4* (5) becomes 1.0 – (20 – 45 % Very strong limitation)
#	4* (6) becomes 0 –(>45% Unsuitable) 


a <- c(1,0.2)
a <- rbind(a,(c(2,0.4)))
a <- rbind(a,(c(3,0.6)))
a <- rbind(a,(c(4,0.8)))
a <- rbind(a,(c(5,1.0)))
a <- rbind(a,(c(6,0)))

OAslope.m <- reclassify(vslope.m, a)

summary(OAslope.m) 

writeRaster(OAslope.m, "Data/OAgri-slope_2018-08-16", "ascii", "overwrite"=T)


unlink("Data/decliv_class.asc")  #large file so delete! 
#unlink("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")


