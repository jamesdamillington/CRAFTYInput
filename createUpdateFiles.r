#create update files from maps 
#only years for which maps exist will be added in given years

rm(list=ls())

library(tidyverse)
library(raster)

scenario <- "testing_2019-11-19h"

update_yrs <- seq(2002, 2018, 1)

#specify csv containing spatially uniform capital values
#each row is a year, each column is Capital 
uniform_caps <- read_csv("Data/UniformCapitals.csv")

#need to use region file to identify required XY cells for this simulation
region <- read_csv("Data/region2001_noDC_HD_2019-11-19h.csv",
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


#specify capital map filename patterns  (assumes all map files end 'YEAR.asc')
mois <- "MoistureCap_OctNovDecJanFebMar_S_"
PortAccess <- "PortAccessCap"
Oagri <- "singleLC_OtherAgri_"
other <- "singleLC_Other_"
Soy_LP <- "Soy_ProtectionMap_"
Pas_LP <- "Pasture_ProtectionMap_"
GrowSeason <- "GSCap_JanFebMarAprMayJun_S_"
human <- "HumanCapital"

#create list of capital maps to work through
#map_caps <- list(agri, infra, Oagri, other)
map_caps <- list(mois, PortAccess, other, Soy_LP, Pas_LP, GrowSeason, human)
#map_caps <- list(PortAccess, other, Soy_LP, Pas_LP)
  
#labels that need to be use for capitals in the final output file
#map_caps_labs <- list("Agriculture", "Infrastructure", "Other Agriculture", "Other")
map_caps_labs <- list("Moisture", "Port Access", "Other", "Soy Protection", "Pasture Protection", "Growing Season", "Human")
#map_caps_labs <- list("Port Access", "Other", "Soy Protection", "Pasture Protection")
  
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
  return(as_tibble(map))  #return xyz as tibble
}


#loop each year
for(i in seq_along(update_yrs)) {
  
  #select only few columns we need (and we drop muniID belo also)
  joined <- dplyr::select(region, Y, X, muniID)
  
  print(update_yrs[i])
  
  #loop each capital
  for(j in seq_along(map_caps)) {
  
    #create file name to check
    filen <- paste0("Data/updates/",map_caps[j],update_yrs[i],".asc")    
  
    #if file exists, extract xyz
    if(file_test("-f",filen)) {

      ras <- raster(filen)
      xy <- readMapXYZ(ras)
      
      #create table for this year
      xy <- xy %>% 
        dplyr::select(-V1) %>%
        dplyr::rename(Y = row, X = col, !!map_caps_labs[[j]] := vals) %>% #see https://stackoverflow.com/a/26003971
        mutate(Y = round(Y,3)) %>%
        mutate(X = round(X,3))
            
      
      joined <- left_join(joined, xy, by = c("Y", "X"))
    }

  }
  
  #select the uniform capital values for this year (should produce a one-row table)
  unis <- uniform_caps %>% 
    filter(Year == !!update_yrs[i])
  
  #repeat the single row to match the number of pixels in the maps above
  unis_rep <- unis %>% 
    slice(rep(1:n(), length(joined$Y)))

  #join the mapped and uniform values
  joined <- cbind(joined, unis_rep)
  
  #some Moisture cells seem to be NA, change those values (also need to do this for GrowSeason?)
  if(exists("Moisture")){
    # for now replace with mean of all non-NA values
    mois_replace <- round(mean(joined$Moisture, na.rm=T),3)
    print(mois_replace)
    
    #replace Moisture NAs
    joined <- joined %>% 
      mutate(Moisture = replace(Moisture, is.na(Moisture), mois_replace))
  }
  
  #drop un-necessary columns and replace Moisture NAs
  joined <- joined %>% 
    dplyr::select(-muniID, -Year)
      

  write_csv(joined,paste0("Data/updates/",scenario,"_update",update_yrs[i],".csv"))
  
  rm(joined)  #remove joined for next year
  
}
      





