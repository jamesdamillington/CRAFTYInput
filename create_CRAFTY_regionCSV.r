#Creating CRAFTY input data - region.csv
#edited from create_CRAFTY_regionCSV2.0_JM_2018-04-27.r 

#The following map data are needed as inputs
# - map of municipality IDs to be simulated
# - land cover from LandCoverMap.r
# - land prices from LandPriceMap.r
# - agriculture capital from agricultureMap.r 
# - infrastrucutre capital from infrastructureMap.r

#script now assumes all these munis files are latlong with identical headers (but not that the spatial extent is identicl)

#Also required:
# Municipality_area_and_IBGE_code_number.csv
# A list of states in which Double Cropping is possible

#Captials derived within this script are:
# - Nature
# - Human
# - Development
# - Economic
# - Climate (although not sure this is needed??)
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
  return(as.tibble(map))  #return xyz as tibble
}
######

#land cover map provided should have 5 landcover classes:
#1 = Nature
#2 = Other Agri
#3 = Agri
#4 = Other
#5 = Pasture

ofname <- "regionPastureB.csv"  #output filename
  
  
#unzip if needed
#unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data") 

#read required files
munis <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc") #map of municipality IDs to be simulated
LC <- raster("Data/LandCover2000_PastureB.asc")  #land cover from LandCoverMap.r
Lpr <- raster('Data/LandPrice2001_Capital.asc')  #land prices from LandPriceMap.r
agri <- raster('Data/agricultureCapital/agricultureCapital2000.asc')   #agriculture capital from agricultureMap.r 
infra <- raster('Data/infrastructureCapital/infrastructureCap1997.asc') #infrastrucutre capital from infrastructureMap.r

#if we want to see input maps
plt <- F
if(plt) {
  plot(munis, main = "munis")
  plot(LC, main = "LC")
  plot(Lpr, main = "Lpr")
  plot(agri, main = "agri")
  plot(infra, main = "infra")
}


#convert raster to xyz
munis.xy <- readMapXYZ(munis)  
LC.xy <- readMapXYZ(LC)  
Lpr.xy <- readMapXYZ(Lpr)  
agri.xy <- readMapXYZ(agri)   
infra.xy <- readMapXYZ(infra) 


#create a list of unique municipality id values
munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")
u.mids <- unique(munis.r)  


#joins (because Infrastructure, Agriculture, Land Price maps are not perfectly aligned with munis.r)
join.xy <- left_join(munis.xy, infra.xy, by = c("row", "col")) %>%
  select(-V1.y) %>%
  rename(muniID = vals.x, Infrastructure = vals.y) %>%
  left_join(., Lpr.xy, by = c("row", "col")) %>%
  select(-V1) %>%
  rename("Land Price" = vals) %>%
  left_join(., agri.xy, by = c("row", "col")) %>%
  select(-V1) %>%
  rename("Agriculture" = vals) %>%  
  filter_all(all_vars(!is.na(.)))      #filter any rows missing data NA values


#DoubleCropping (used for other variables below, but ultimately removed?)
state <-read_csv("Data/Municipality_area_and_IBGE_code_number.csv")
#Fstate_vals <- c(17,	29,	31,	35,	41,	42,	43,	50,	51,	52)

DCstates <- c(50,51,41,52,31,35)  #specify which states double cropping is possible in
DC <- inner_join(join.xy, state, by = c("muniID" = "CD_GCMUN")) %>%
  select(row, col, CD_GCUF) %>%
  mutate("Growing Season" = if_else(CD_GCUF %in% DCstates,  1, 0))  #1 if stateID is in list otherwise 0
      
join.xy <- left_join(join.xy, DC, by = c("row","col")) %>%
  select(-CD_GCUF)


#add LC to the table then use to create FR column (see below for logic)
join.xy <- left_join(join.xy, LC.xy, by = c("row","col")) %>%
  select(-V1) %>%
  rename(LC = vals) %>%
  mutate(FR = 
    if_else(LC == 1, "FR5",          #Nature        
    if_else(LC == 2, "FR6",           #Other Agri      
    if_else(LC == 3,                 #Agri
      if_else(`Growing Season` == 1, "FR3",         #if double cropping possible, always assign
        if_else(rbinom(n(),1,0.5) == 1,"FR1","FR2")),   #if double cropping not possible, randomly assign soy or maize (or should weight by muncipality data on production??) see https://stackoverflow.com/a/31878476 for n()
    if_else(LC == 4, "FR7", "FR8")   #Other or Pasture
    ))))
    
#FR1 = Soy (LC3 where DC == 0)
#FR2 = Maize (LC3 where DC == 0)
#FR3 = Double Crop (soy/maize) (LC3 where DC == 1)
#FR4 = Nature (LC1) [does not exist at model initialisation]
#FR5 = Stubborn Nature (LC1)
#FR6 = Other Agri (LC2)
#FR7 = Other (LC4)
#FR8 = Pasture (LC5) 

#add nature
join.xy <- join.xy %>%
  mutate(Nature = if_else(FR == "FR5", 0.9,       #virgin nature (FR4 not in initial state)      
    if_else(FR == "FR8", 0.6,                     #pasture
    if_else(FR == "FR7", 0, 0.3)                  #other = 0.0, non-pasture agri uses = 0.3
    )))

#add accessibility (NB note typo) (but this is calculated dynamically within CRAFY code)
join.xy <- join.xy %>%
  mutate(Acessibility = if_else(FR == "FR5",
    sample(1:20,n(),replace=T)/100,
    sample(90:100,n(),replace=T)/100
  ))


#add columns that are either uniform or simple row number
region.xy <- join.xy %>%
  rename(Y = row, X = col) %>%
  mutate(agentid = row_number()) %>%  #add dummy agent_ID
  mutate(BT = "Cognitor") %>%
  mutate(Human = 1.0) %>%
  mutate(Development = 1.0) %>%
  mutate(Economic = 1.0) %>%
  mutate(`Land Protection` = 1.0) %>%
  mutate(Other = if_else(FR == "FR7", 1.0,0)) %>%                #if Other LC set Capital to 1
  mutate(`Other Agriculture` = if_else(FR == "FR6", 1.0,0)) %>%  #if Other Agri LC set Capital to 1
  mutate(Climate = Agriculture) %>%
  mutate(Infrastructure = round(Infrastructure, 3))  #added to prevent many dps (unknown why)

region <- region.xy %>% 
  filter_all(., all_vars(!is.na(.))) %>%
  select(V1.x,Y,X,muniID,BT,FR,agentid,Agriculture,Nature,Human,Development,Infrastructure,Economic,Acessibility,Climate,`Land Price`,`Growing Season`,`Other Agriculture`,Other,`Land Protection`) %>%
  rename(" " = V1.x)

write_csv(region, paste0("Data/",ofname))

