#short script to modify agricultureCapital map

rm(list=ls())
library(raster)

sim_yrs <- seq(2001, 2016, 1)

#loop each year
for(i in seq_along(sim_yrs)) {
  
  #create file name to check
  filen <- paste0("Data/agricultureCapital/agricultureCapital",sim_yrs[i],".asc")  
  
  ras <- raster(filen)
  
  rasp2 <- ras + 0.2        #add 0.2 to all cells
  rasp2[rasp2 > 1] <- 1     #but keep values of 1 
  rasp2[rasp2 == 0.2] <- 0  #and keep values of 0
  
  rasp2 <- round(rasp2,1)  #round to 1 d.p.
  
  writeRaster(rasp2, paste0("Data/agricultureCapital/agricultureCapital_p02_",sim_yrs[i],".asc"), format = 'ascii', overwrite=T)
}
