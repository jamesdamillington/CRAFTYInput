
rm(list=ls())

library(tidyverse)
library(raster)

scenario <- "Testing"

sim_yrs <- seq(2001, 2002, 1)

#specify capital filename patterns  (assumes all end 'YEAR.asc'
agri <- "agricultureCapital" 
infra <- "infrastructureCap"
Oagri <- "singleLC_OtherAgri_"
other <- "singleLC_Other_"

#create list of capitals to work through
caps <- list(agri, infra, Oagri, other)

#labels that need to be use for capitals in the final output file
caps_labs <- list("Agriculture", "Infrastructure", "Other Agriculture", "Other")


#FUNCTIONS
#raster to xyz  (with help from https://stackoverflow.com/a/19847419)
#sepcify input raster, whether nodata cells should be output, whether a unique cell ID should be added
#return is a matrix. note format is row (Y) then col (X)
extractXYZ <- function(raster, nodata = FALSE, addCellID = TRUE){
  
  vals <- raster::extract(raster, 1:ncell(raster))   #specify raster otherwise dplyr used
  xys <- rowColFromCell(raster,1:ncell(raster))
  combine <- cbind(xys,vals)
  
  if(addCellID){
    combine <- cbind(1:length(combine[,1]), combine)
  }
  
  if(!nodata){
    combine <- combine[!rowSums(!is.finite(combine)),]  #from https://stackoverflow.com/a/15773560
  }
  
  return(combine)
}

#read raster data function
readMapXYZ <- function(mapz)
{
  #map <- raster(path)     #read raster
  map <-flip(mapz, 'y')    #flip maps as CRAFTY read values from the bottom of the map
  map <- extractXYZ(map)  #convert from map to xyz (as tibble)
  return(as.tibble(map))  #return xyz as tibble
}



munis <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc") #map of municipality IDs to be simulated
munis.xy <- readMapXYZ(munis)  

#loop each year
for(i in seq_along(sim_yrs)) {
  
  print(sim_yrs[i])
  
  #loop each capital
  for(j in seq_along(caps)) {
  
    #create file name to check
    filen <- paste0("Data/updates/",caps[j],sim_yrs[i],".asc")    
  
    #if file exists, extract xyz
    if(file_test("-f",filen)) {

      ras <- raster(filen)
      xy <- readMapXYZ(ras)
      
      #create table for this year
      xy <- xy %>% 
        dplyr::select(-V1) %>%
        dplyr::rename(Y = row, X = col, !!caps_labs[[j]] := vals)  #see https://stackoverflow.com/a/26003971
      
      #if we have already created a table, join this new one, else assign xy to joined
      joined <- if(exists("joined")) left_join(joined, xy, by = c("Y", "X")) else xy 

    }

  }
  
  write_csv(joined,paste0("Data/updates/",scenario,"_update",sim_yrs[i],".csv"))
  
  rm(joined)  #remove joined for next year
  
}
      





