#create update files from maps 
#only years for which maps exist will be added in given years


rm(list=ls())

library(tidyverse)
library(raster)

scenario <- "ClimateRCP85_DC"

sim_yrs <- seq(2031, 2050, 1)

#specify capital filename patterns  (assumes all end 'YEAR.asc'
agri <- "agricultureCapital_rcp85_" 
PortAccess <- "PortAccessCap"
Oagri <- "singleLC_OtherAgri_"
other <- "singleLC_Other_"
Soy_LP <- "Soy_ProtectionMap_"
Pas_LP <- "Pasture_ProtectionMap_"
GrowSeason <- "DC_rcp85_"

#need to use region file to identify required XY cells for this simulation
region <- read_csv("Data/region2015.csv",
  col_types = cols(`Land Price` = col_double(),
    `Growing Season` = col_double(),
    Other  = col_double(),
    `Soy Protection` = col_double(),
    `Maize Protection` = col_double(),
    `Pasture Protection` = col_double(),
    `OAgri Protection` = col_double(),
    `Agri Infrastructure` = col_double(),
    `OAgri Infrastructure` = col_double()
    )
  ) #needed to read correct type


#create list of capitals to work through
#caps <- list(agri, infra, Oagri, other)
#caps <- list(agri, PortAccess, other, Soy_LP, Pas_LP)
caps <- list(agri, GrowSeason)
  
#labels that need to be use for capitals in the final output file
#caps_labs <- list("Agriculture", "Infrastructure", "Other Agriculture", "Other")
#caps_labs <- list("Agriculture", "Port Access", "Other", "Soy Protection", "Pasture Protection")
caps_labs <- list("Agriculture", "Growing Season")
  
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
  map <- round(map, 3)
  return(as.tibble(map))  #return xyz as tibble
}


#loop each year
for(i in seq_along(sim_yrs)) {
  
  #select only few columns we need (and we drop muniID belo also)
  joined <- dplyr::select(region, Y, X, muniID)
  
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
        dplyr::rename(Y = row, X = col, !!caps_labs[[j]] := vals) %>% #see https://stackoverflow.com/a/26003971
        mutate(Y = round(Y,3)) %>%
        mutate(X = round(X,3))
            
      
      joined <- left_join(joined, xy, by = c("Y", "X"))
    }

  }
  
  joined <- joined %>% dplyr::select(-muniID)
  write_csv(joined,paste0("Data/updates/",scenario,"_update",sim_yrs[i],".csv"))
  
  rm(joined)  #remove joined for next year
  
}
      





