#Creating CRAFTY input data - region.csv
#edited from create_CRAFTY_regionCSV2.0_JM_2018-04-27.r 

#The following map data are needed as inputs
# - map of municipality IDs to be simulated
# - land cover from LandCoverMap.r (or ClassifyDisaggregateMap.r)
# - land prices from LandPriceMap.r
# - moisture and growing season capital maps from moistureMap.r 
# - infrastrucutre capital from infrastructureMap.r

#script now assumes all these munis files are latlong with identical headers (but not that the spatial extent is identical)

#Also required:
# Municipality_area_and_IBGE_code_number.csv
# A list of states in which Double Cropping is possible

#Captials derived within this script are:
# - Moisture
# - Nature
# - Human
# - Development
# - Economic
# - Other 
# - Other Agriculture
# - Land Protection

rm(list=ls())

library(raster)
library(tidyverse)


######
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
  return(as_tibble(map))  #return xyz as tibble
}


recode_if <- function(x, condition, ...){
  if_else(condition, recode(x, ...), x)
}
######

#land cover map provided should have 5 landcover classes:
#1 = Nature
#2 = Other Agri
#3 = Agri
#4 = Other
#5 = Pasture

ofname <- "region2001_2020-04-18.csv"  #output filename
  
  
#unzip if needed
#unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data") 

#read required files
munis <- raster("Data/sim10_BRmunis_latlon_5km.asc") #map of municipality IDs to be simulated
#LC <- raster("Data/ObservedLCmaps/LandCover2001_PastureB_Disagg.asc")  #land cover from LandCoverMap.r (or ClassifyDisaggregateMap.r)
LC <- raster("Data/ObservedLCmaps/LandCover2001_PastureB_Disagg.asc")  #land cover from LandCoverMap.r (or ClassifyDisaggregateMap.r)
#Lpr <- raster('Data/LandPrice2001_Capital_nat1.asc')  #land prices from LandPriceMap.r
Lpr <- raster('Data/LandPrice/LandPrice_Capital_08_2001.asc')  #land prices from LandPriceMap.r

#mois <- raster('Data/Moisture/MoistureCap_OctNovDecJanFebMar_S_2001.asc')   #moisture capital from moistureMap.r 
mois <- raster('Data/Moisture/MoistureCap_OctNovDecJanFebMar_S_2001.asc')   #moisture capital from moistureMap.r 
#GS <- raster('Data/Moisture/GSCap_JanFebMarAprMayJun_S_2001.asc')   #growing season (double cropper) capital from moistureMap.r 
GS <- raster('Data/Moisture/GSCap_JanFebMarAprMayJun_S_2001.asc')   #growing season (double cropper) capital from moistureMap.r 

#infra <- raster('Data/PortAccessCapital/PortAccessCap2000.asc') #infrastrucutre capital from infrastructureMap.r
infra <- raster('Data/PortAccessCapital/PortAccessCap2020_2000.asc') #infrastrucutre capital from infrastructureMap.r
#SoyProtect <- raster('Data/landProtection/All_ProtectionMap.asc') #land protection is intially identical for all services
#MaizeProtect <- raster('Data/landProtection/All_ProtectionMap.asc') #land protection is intially identical for all services
#OAgriProtect <- raster('Data/landProtection/All_ProtectionMap.asc') #land protection is intially identical for all services
#PasProtect <- raster('Data/landProtection/All_ProtectionMap.asc') #land protection is intially identical for all services
SoyProtect <- raster('Data/landProtection/All_ProtectionMap_025.asc') 
MaizeProtect <- raster('Data/landProtection/All_ProtectionMap_025.asc') 
OAgriProtect <- raster('Data/landProtection/All_ProtectionMap_025.asc') 
PasProtect <- raster('Data/landProtection/All_ProtectionMap_025.asc') 

Economic <- raster('Data/AgriLocations2001.asc')

OAslope <- raster('Data/OAgri-slope_2018-08-16.asc')  #other agriculture cap set to slope 
OAslope <- round(OAslope, 1)  #round because of long dp

NatAccess <- raster('Data/NatureAccess_2001_PastureB_Disagg_075.asc') 
AgriAccess <- raster('Data/AgriAccess_2001_PastureB_Disagg_005.asc') 
OAgriAccess <- raster('Data/OAgriAccess_2001_PastureB_Disagg_005.asc') 
 
agriHarvest <- read_csv("Data/muni2001_harvestAreas.csv", col_types = ("iiiid")) #from DoubleCropping.rmd
DC <- F   #NO double cropping exists in 2001

#agriHarvest <- read_csv("Data/muni2018_plantedAreas_newDC.csv", col_types = ("iiiiiidddd")) #from DoubleCropping.rmd
#DC <- T   #double cropping exists in 2018

VH <- F
#specify csv containing spatially uniform capital values (as used in createUpdateFiles.r)
uniform_caps <- read_csv("Data/UniformCapitals_2020-02-10b.csv")
unis <- uniform_caps %>% 
  filter(Year == 2001)   #select the uniform capital values for start year (edit if starting from a different year)


#if we want to see input maps
plt <- F
if(plt) {
  plot(munis, main = "munis")
  plot(LC, main = "LC")
  plot(Lpr, main = "Lpr")
  plot(mois, main = "moisture")
  plot(GS, main = "growing season")
  plot(infra, main = "Port Access")
  plot(SoyProtect, main = "SoyProtect")
  plot(MaizeProtect, main = "MaizeProtect")
  plot(OAgriProtect, main = "OAgriProtect")
  plot(PasProtect, main = "PasProtect")
}


#convert raster to xyz
munis.xy <- readMapXYZ(munis)  
LC.xy <- readMapXYZ(LC)  
Lpr.xy <- readMapXYZ(Lpr)  
mois.xy <- readMapXYZ(mois) 
GS.xy <- readMapXYZ(GS)   
infra.xy <- readMapXYZ(infra) 
SoyProtect.xy <- readMapXYZ(SoyProtect)
MaizeProtect.xy <- readMapXYZ(MaizeProtect)
OAgriProtect.xy <- readMapXYZ(OAgriProtect)
PasProtect.xy <- readMapXYZ(PasProtect)
OAslope.xy <- readMapXYZ(OAslope) 
NatAccess.xy <- readMapXYZ(NatAccess) 
AgriAccess.xy <- readMapXYZ(AgriAccess) 
OAgriAccess.xy <- readMapXYZ(OAgriAccess) 
Economic.xy <- readMapXYZ(Economic) 

#create a list of unique municipality id values
u.mids <- unique(munis)  


#joins (because Infrastructure, Moisture, Land Price maps are not perfectly aligned with munis)
#four service land protections use the same initial conditions
join.xy <- left_join(munis.xy, infra.xy, by = c("row", "col")) %>%
  dplyr::select(-V1.y) %>%
  rename(muniID = vals.x, `Port Access` = vals.y) %>%
  left_join(., round(Lpr.xy,3), by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Land Price" = vals) %>%
  left_join(., round(mois.xy,3), by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Moisture" = vals) %>%  
  left_join(., round(GS.xy,3), by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Growing Season" = vals) %>% 
  left_join(., SoyProtect.xy, by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Soy Protection" = vals) %>%  
  left_join(., MaizeProtect.xy, by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Maize Protection" = vals) %>% 
  left_join(., PasProtect.xy, by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Pasture Protection" = vals) %>% 
  left_join(., OAgriProtect.xy, by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("OAgri Protection" = vals) %>% 
  left_join(., OAslope.xy, by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Other Agriculture" = vals) %>% 
  left_join(., NatAccess.xy, by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Nature Access" = vals) %>% 
  left_join(., AgriAccess.xy, by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Agri Infrastructure" = vals) %>% 
  left_join(., OAgriAccess.xy, by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("OAgri Infrastructure" = vals) %>% 
  left_join(., round(Economic.xy,3), by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Economic" = vals) 

if(VH){
  human <- raster('Data/HumanDev/HumanCapital2001.asc') 
  human.xy <- readMapXYZ(human) 
  
  join.xy <- left_join(munis.xy, human.xy, by = c("row", "col")) %>%
  dplyr::select(-V1) %>%
  rename("Human" = vals) %>% 
  filter_all(all_vars(!is.na(.)))      #filter any rows missing data NA values
}
  
if(DC){
#add harvest areas and LC to the table then use to create FR column (see below for logic)
join.xy <- agriHarvest %>%
  dplyr::select(muniID, maize_prop, cum_ms_prop, dc_prop) %>%
  full_join(., join.xy, by = c("muniID")) 

join.xy <- left_join(join.xy, LC.xy, by = c("row","col")) %>%
  dplyr::select(-V1) %>%
  rename(LC = vals) %>%
  mutate(rand = runif(n(),0,1)) %>%
  mutate(FR = 
    if_else(LC == 1, "FR5",          #Nature        
    if_else(LC == 2, "FR6",           #Other Agri      
    if_else(LC == 3,                 #Agri #proportions calculated in DoubleCropping.rmd
      if_else(rand <= maize_prop, "FR2", 
          if_else(rand <= cum_ms_prop,  "FR1",
              if_else(dc_prop > 0, "FR3",
                if_else(rand < 0.5, "FR2", "FR1")))),  #because there are some munis mapped that for some reason are not in the IBGE data...
    if_else(LC == 4, "FR7", "FR8")   #Other or Pasture
    ))))
}

if(!DC){
  
  join.xy <- agriHarvest %>%
  dplyr::select(muniID, maize_prop) %>%
  full_join(., join.xy, by = c("muniID")) 
  
  join.xy <- left_join(join.xy, LC.xy, by = c("row","col")) %>%
  dplyr::select(-V1) %>%
  rename(LC = vals) %>%
  mutate(rand = runif(n(),0,1)) %>%
  mutate(FR = 
    if_else(LC == 1, "FR5",          #Nature        
    if_else(LC == 2, "FR6",           #Other Agri      
    if_else(LC == 3,                 #Agri #proportions calculated in DoubleCropping.rmd
      if_else(rand <= maize_prop, "FR2", "FR1"),  
    if_else(LC == 4, "FR7", "FR8")   #Other or Pasture
    ))))
}

#FR1 = Soy 
#FR2 = Maize
#FR3 = Double Crop 
#FR4 = Nature (LC1) [does not exist at model initialisation]
#FR5 = Nature (LC1)
#FR6 = Other Agri (LC2)
#FR7 = Other (LC4)
#FR8 = Pasture (LC5) 

#add nature
join.xy <- join.xy %>%
  mutate(Nature = if_else(FR == "FR5", 1,       #virgin nature (FR4 not in initial state)      
    if_else(FR == "FR8", 0.4,0)                     #pasture = 0.4, else 0
    ))

#decrease Nature Capital at edge (Nature Access as guide)
join.xy <- join.xy %>%
  mutate(Nature = if_else(Nature == 1 & `Nature Access` == 0.75, 0.75, Nature))

#write.table(join.xy, file=paste0("Data/","joinxy-",ofname), sep=",",row.names=F) #use write.table to wrap 'Cognitor' and column headers in quotes 


#add columns that are either uniform or simple row number
region.xy <- join.xy %>%
  rename(Y = row, X = col) %>%
  mutate(agentid = row_number()) %>%  #add dummy agent_ID
  mutate(BT = "Cognitor") %>%
  mutate(Development = unis$Development) %>%
  #mutate(Economic = 1.0) %>%
  mutate(Other = if_else(FR == "FR7", 1,0)) %>%                #if Other LC set Capital to 1
  mutate(Climate = Moisture) %>%
  mutate(`Port Access` = round(`Port Access`, 3))  #added to prevent many dps (unknown why)

if(!VH){
  region.xy <- mutate(region.xy, Human = unis$Human)
}

region <- region.xy %>% 
  dplyr::select(Y,X,muniID,BT,FR,Moisture,Nature,Human,Development,`Port Access`,Economic,`Nature Access`,`Land Price`,`Growing Season`,`Other Agriculture`,Other,`Soy Protection`,`Maize Protection`,`Pasture Protection`,`OAgri Protection`,`Agri Infrastructure`,`OAgri Infrastructure`) %>%
  filter_all(., all_vars(!is.na(.))) #%>%
  #rename(" " = V1.x) 

write.table(region, file=paste0("Data/",ofname), sep=",",row.names=F) #use write.table to wrap 'Cognitor' and column headers in quotes 

