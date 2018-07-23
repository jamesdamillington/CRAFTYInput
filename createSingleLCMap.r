#create maps of a single LC from observed LC maps
#useful for updating Other Agri and Other Capitals through simulation

rm(list=ls())
library(raster)

sim_yrs <- seq(2000, 2015, 1)   #movie made for all these years

#1 = Nature
#2 = Other Agri
#3 = Agri
#4 = Other
# = Pasture

#what is desired output LC?
target = 4

#target name (for outpu file name)
tname <- "Other"

#if binary true output a binary map where 1 = target LC and 0 is not
#if binary false, output map where target LC value is maintained, all others set to 0
binary = T

#create df for subs below
df <- data.frame(id=1:5, v=rep.int(0,5))
df[target,2] <- if(binary) 1 else target


sim_yrs <- c(2005, 2010, 2015)   #output for all these years

for(i in seq_along(sim_yrs)){

  ObsLU <- raster(paste0("Data/ObservedLCmaps/brazillc_",sim_yrs[i],"_PastureB.asc"))

  maskmap <- raster(paste0("Data/ObservedLCmaps/sim10_BRmunis_latlon_5km_2018-04-27.asc"))
  
  LU <- mask(ObsLU, maskmap)
  
  LU <- subs(LU, df)
  
  writeRaster(LU, paste0("Data/singleLC_",tname,"_",sim_yrs[i],".asc"), format = 'ascii', overwrite=T)
  
}

   

