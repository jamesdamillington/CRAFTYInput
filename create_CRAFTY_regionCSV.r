#Creating CRAFTY input data - region.csv
#edited from create_CRAFTY_regionCSV2.0_JM_2018-04-27.r 



#list here what Captial maps shoudle be provided (and in what scripts they are created)
#also list what Captials are derived within this script


#!!needs checking and cleaning!!

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



#read raster data
#script now assumes all these munis files are latlong with identical headers

#read munis.r as latlong
unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data")  #unzip
munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")
#latlong <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
#crs(munis.r) <- latlong


#land cover map provided should have 5 landcover classes:
#1 = Nature
#2 = Other Agri
#3 = Agri
#4 = Other
#5 = Pasture

landCover.r <- raster("Data/LandCover2000_PastureB.asc")


#create a list of unique municipality id values
u.mids <- unique(munis.r)  #JM edit munis.r
#length(u.mids)


#read required files
#land prices from LandPriceMap.r
Lpr.r <- raster('Data/LandPrice2001_Capital.asc')

#classified soil map from soilMap.r
soil.r <- raster('Data/vsoil_2018-05-08.asc')   

#classfied slope values from slopeMap.r 
slope.r <- raster("Data/vslope_2018-04-30.asc")

#this is the agriculture capital map. made in agricultureMap.r 
agri.r <- raster('Data/agricultureCapital/agricultureCapital2000.asc')   

#infrastrucutre Map made in infrastructureMap.r
infra.r <- raster('Data/infrastructureCapital/infrastructureCap1997.asc')

#flip maps as CRAFTY read values from the bottom of the map
munis.r<-flip(munis.r, 'y')
infra.r<-flip(infra.r, 'y')
soil.r<-flip(soil.r, 'y')
agri.r<-flip(agri.r, 'y')
slope.r<-flip(slope.r, 'y')
landCover.r <-flip(landCover.r, 'y')
Lpr.r<-flip(Lpr.r, 'y')


munis.xy <- as.tibble(extractXYZ(munis.r))
LC.xy <- as.tibble(extractXYZ(landCover.r))
Lpr.xy <- as.tibble(extractXYZ(Lpr.r))
soil.xy <- as.tibble(extractXYZ(soil.r))
slope.xy <- as.tibble(extractXYZ(slope.r))
agri.xy <- as.tibble(extractXYZ(agri.r))
infra.xy <- as.tibble(extractXYZ(infra.r))

#create function to do each of above:
#1. load raster
#2. flip
#3. extract and return as tibble



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
    if_else(LC == 1, "FR5",                 
    if_else(LC == 2, "FR6",                 
    if_else(LC == 3,
      if_else(`Growing Season` == 1, "FR3",         #if double cropping possible, always assign
        if_else(rbinom(n(),1,0.5) == 1,"FR1","FR2")),   #if double cropping not possible, randomly assign soy or maize (or should weight by muncipality data on production??) see https://stackoverflow.com/a/31878476 for n()
    if_else(LC == 4, "FR7", "FR8")
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
  mutate(Other = if_else(FR == "FR7", 1.0,0)) %>%
  mutate(`Other Agriculture` = if_else(FR == "FR6", 1.0,0)) %>%
  mutate(Climate = Agriculture)

region <- region.xy %>% 
  filter_all(., all_vars(!is.na(.))) %>%
  select(V1.x,Y,X,muniID,BT,FR,agentid,Agriculture,Nature,Human,Development,Infrastructure,Economic,Acessibility,Climate,`Land Price`,`Growing Season`,`Other Agriculture`,Other,`Land Protection`) %>%
  rename(" " = V1.x)

write_csv(region, "Data/region.csv")


  
#Acessibility	
#Climate	     #not needed??


#Growing Season	
#Other Agriculture	
#Other	
#Land Protection
  
#old code below. comment out as implemented above
#muniscsv$FR[muniscsv$Landuse==1]<-5
#muniscsv$FR[muniscsv$Landuse==2]<-6
#muniscsv$FR[muniscsv$Landuse==3]<-sample(1:3, nrow(muniscsv), replace=T)
#muniscsv$FR[muniscsv$Landuse==4]<-7
#muniscsv$FR[muniscsv$FR==5]<-"FR5"
#muniscsv$FR[muniscsv$FR==3]<-"FR3"
#muniscsv$FR[muniscsv$FR==2]<-"FR2"
#muniscsv$FR[muniscsv$FR==1]<-"FR1"
#muniscsv$FR[muniscsv$FR==7]<-"FR7"
#muniscsv$FR[muniscsv$FR==6]<-"FR6"

#muniscsv$BT<-"Cognitor"
#muniscsv$agentid<-1:nrow(muniscsv)
#names(muniscsv)[1]="ID"
#names(muniscsv)[2]="Y"
#names(muniscsv)[3]="X"
#names(muniscsv)[20]="Land Price"
#muniscsv$Acessibility<-(sample(90:100, size=nrow(muniscsv), replace = T))/100
#muniscsv$Acessibility[muniscsv$FR=="FR5"]<-(sample(1:20, size=nrow(muniscsv), replace = T))/100
#muniscsv$Infrastructure<-muniscsv$Ports
#muniscsv$Soil[muniscsv$Slope==0]<-0
#muniscsv$Slope[muniscsv$Soil==0]<-0
#muniscsv$Climate[muniscsv$Slope==0]<-0
#muniscsv$Climate[muniscsv$Soil==0]<-0
#muniscsv$Agriculture<-muniscsv$Climate
#muniscsv$Human<-1
#muniscsv$Development<-1
#muniscsv$Economic<-1
#muniscsv$Nature[muniscsv$FR=="FR5"]<-0.9
#muniscsv$Nature[muniscsv$FR=="FR4"]<-0.9
#muniscsv$Nature[muniscsv$FR=="FR3"]<-0.3
#muniscsv$Nature[muniscsv$FR=="FR2"]<-0.3
#muniscsv$Nature[muniscsv$FR=="FR1"]<-0.3
#muniscsv$Nature[muniscsv$FR=="FR6"]<-0.6
#muniscsv$Nature[muniscsv$FR=="FR7"]<-0
#muniscsv$"Growing Season"<-0
#muniscsv$Pasture<-0
#muniscsv$Pasture[muniscsv$FR=="FR6"]<-1
#muniscsv$Other<-0
#muniscsv$Other[muniscsv$FR=="FR7"]<-1
#muniscsv$"Land Protection"<-1
#muniscsv$`Land Price`<-signif(muniscsv$`Land Price`, digits=3)
#muniscsv$`Land Price`<-1500-muniscsv$`Land Price`
#muniscsv$`Land Price`<-muniscsv$`Land Price`/1500
#muniscsv$Ports<-NULL
#muniscsv$Soil<-NULL
#muniscsv$Slope<-NULL
#muniscsv$Landuse<-NULL
#names(statecsv)[5]="muniID"
#bzro<-inner_join(statecsv, muniscsv, by = "muniID")
#statedata<-bzro[, c(2,5)]
#statedata$DoubleCropping<-0
#statedata$DoubleCropping[statedata$CD_GCUF==50]<-1
#statedata$DoubleCropping[statedata$CD_GCUF==51]<-1
#statedata$DoubleCropping[statedata$CD_GCUF==41]<-1
#statedata$DoubleCropping[statedata$CD_GCUF==52]<-1
#statedata$DoubleCropping[statedata$CD_GCUF==31]<-1
#statedata$DoubleCropping[statedata$CD_GCUF==35]<-1
#statedata$CD_GCUF<-NULL
#statedata<-statedata[!duplicated(statedata$muniID),]
#muniscsv<-inner_join(muniscsv, statedata, by = "muniID")

#names(muniscsv)[1]=""
#muniscsv$`Growing Season`[muniscsv$DoubleCropping==1]<-1
#muniscsv$DoubleCropping<-NULL
#muniscsv$FR[(muniscsv$FR=="FR1"|muniscsv$FR=="FR2")&(muniscsv$`Growing Season`==1)]<-"FR3"
#muniscsv$FR[(muniscsv$FR=="FR3")&(muniscsv$`Growing Season`==0)]<-sample(1:2, nrow(muniscsv), replace=T)
#muniscsv$FR[muniscsv$FR==2]<-"FR2"
#muniscsv$FR[muniscsv$FR==1]<-"FR1"
#write.csv(muniscsv,"region.csv", row.names = F)



