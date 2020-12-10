#script to create moisture capital map for testing (for % chg) 

#requires climate data from cru_ts4.zip (via https://crudata.uea.ac.uk/cru/data/hrg/)

rm(list = ls())
library(raster)
library(tidyverse)


#set scenario parms
initial_yr <- 2018
final_yr <- 2035
perc_chg <- -20   #this is the % difference over the entire period
additional_yr <- T  #if true generate a map for the year after the final year 

count_yrs <- final_yr - initial_yr
final_diffc <- 1 + (perc_chg / 100)  #convert to final proportion diffc (over entire period)

#read update file
input_scenario <- "Data/Moisture/GSCap_JanFebMarAprMayJun_S_"
#input_scenario <- "Data/Moisture/GSCap_JanFebMarAprMayJun_S_"
#input_scenario <- "Data/Moisture/test_"
initial_map <- raster(paste0(input_scenario,initial_yr,".asc"))

#calculate final year values
final_map <- round(initial_map * final_diffc,3)
final_map[final_map < 0] <- 0
final_map[final_map > 1] <- 1

delta_map <- (final_map - initial_map) / count_yrs  #change by this amount each year (per pixel)

#plot(initial_map,main=initial_yr)

#loop over all years using delta_map to change each year
for(yr in seq(from=initial_yr+1,to=final_yr-1,by=1)){
  
  initial_map <- round(initial_map + delta_map,3)
  initial_map[initial_map < 0] <- 0
  initial_map[initial_map > 1] <- 1
  
  #plot(initial_map,main=yr)
  #m <- cellStats(initial_map, 'mean', na.rm=T)
  print(yr)
  
  writeRaster(initial_map,paste0(input_scenario,"testing",perc_chg,"_",yr,".asc"))  
  
}

writeRaster(final_map,paste0(input_scenario,"testing",perc_chg,"_",final_yr,".asc"))  
#plot(final_map,main=final_yr)


if(additional_yr){
  
  add_yr <- final_yr+1
  add_map <- round(final_map + delta_map,3)
  add_map[add_map < 0] <- 0
  add_map[add_map > 1] <- 1
  
  print(add_yr)
  writeRaster(add_map,paste0(input_scenario,"testing",perc_chg,"_",add_yr,".asc"))  
  
}
