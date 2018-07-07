#Creating CRAFTY input data - region.csv
#edited from create_CRAFTY_regionCSV2.0_JM_2018-04-27.r 


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

landCover.r <- raster("Data/brazillc_2000_int_reclass_5km_txt_pasture.txt")


#classify land cover map here??


#create a list of unique municipality id values
u.mids <- unique(munis.r)  #JM edit munis.r
#length(u.mids)


#read required files
#land prices from LandPriceMap.r
Lpr.r <- raster('Data/LandPrice2001.asc')

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


#create reclassify matrix: muni ids and a random value (uniform for entire muni)
set.seed(1)

statecsv<-read.csv("Municipality_area_and_IBGE_code_number.csv")

munis.xy <- as.tibble(extractXYZ(munis.r))
LC.xy <- as.tibble(extractXYZ(landCover.r))
Lpr.xy <- as.tibble(extractXYZ(Lpr.r))
soil.xy <- as.tibble(extractXYZ(soil.r))
slope.xy <- as.tibble(extractXYZ(slope.r))
agri.xy <- as.tibble(extractXYZ(agri.r))
infra.xy <- as.tibble(extractXYZ(infra.r))


#joins (because some maps are not perfectly aligned with munis.r
join.xy <- left_join(munis.xy, infra.xy, by = c("row", "col")) %>%
  select(-V1.y) %>%
  rename(muniID = vals.x, Infrastructure = vals.y) %>%
  left_join(., Lpr.xy, by = c("row", "col")) %>%
  select(-V1) %>%
  rename("Land Price" = vals) #%>%



region.xy <- join.xy %>%
  rename(" " = V1, Y = row.x, X = col.x) %>%
  mutate(agentID = row_number()) %>%  #add dummy agent_ID
  mutate(BT = "Cognitor") %>%
  
  ##FR here
  
  mutate(Agriculture = agri.xy$vals)  %>%

##Nature here
  
  mutate(Human = 1) %>%
  mutate(Development = 1) %>%
  mutate(Economic = 1) %>%
  
#Acessibility	
#Climate	     #not needed??

  
  mutate(`Land Price` = Lpr.xy$vals)
#Growing Season	
#Other Agriculture	
#Other	
#Land Protection
  




munis.xy <- munis.xy %>%
  mutate(LC = if_else(LCa == "LC1area", 1, 
      if_else(LCa == "LC2area", 2,
      if_else(LCa == "LC3area", 3,
      if_else(LCa == "LC4area", 4,
      if_else(LCa == "LC5area", 5, 0)
      )))))






muniscsv$FR[muniscsv$Landuse==1]<-5
muniscsv$FR[muniscsv$Landuse==2]<-6
muniscsv$FR[muniscsv$Landuse==3]<-sample(1:3, nrow(muniscsv), replace=T)
muniscsv$FR[muniscsv$Landuse==4]<-7
muniscsv$FR[muniscsv$FR==5]<-"FR5"
muniscsv$FR[muniscsv$FR==3]<-"FR3"
muniscsv$FR[muniscsv$FR==2]<-"FR2"
muniscsv$FR[muniscsv$FR==1]<-"FR1"
muniscsv$FR[muniscsv$FR==7]<-"FR7"
muniscsv$FR[muniscsv$FR==6]<-"FR6"

#muniscsv$BT<-"Cognitor"
#muniscsv$agentid<-1:nrow(muniscsv)
#names(muniscsv)[1]="ID"
#names(muniscsv)[2]="Y"
#names(muniscsv)[3]="X"
#names(muniscsv)[20]="Land Price"
muniscsv$Acessibility<-(sample(90:100, size=nrow(muniscsv), replace = T))/100
muniscsv$Acessibility[muniscsv$FR=="FR5"]<-(sample(1:20, size=nrow(muniscsv), replace = T))/100
#muniscsv$Infrastructure<-muniscsv$Ports
#muniscsv$Soil[muniscsv$Slope==0]<-0
#muniscsv$Slope[muniscsv$Soil==0]<-0
#muniscsv$Climate[muniscsv$Slope==0]<-0
#muniscsv$Climate[muniscsv$Soil==0]<-0
#muniscsv$Agriculture<-muniscsv$Climate
#muniscsv$Human<-1
#muniscsv$Development<-1
#muniscsv$Economic<-1
muniscsv$Nature[muniscsv$FR=="FR5"]<-0.9
muniscsv$Nature[muniscsv$FR=="FR4"]<-0.9
muniscsv$Nature[muniscsv$FR=="FR3"]<-0.3
muniscsv$Nature[muniscsv$FR=="FR2"]<-0.3
muniscsv$Nature[muniscsv$FR=="FR1"]<-0.3
muniscsv$Nature[muniscsv$FR=="FR6"]<-0.6
muniscsv$Nature[muniscsv$FR=="FR7"]<-0
muniscsv$"Growing Season"<-0
muniscsv$Pasture<-0
muniscsv$Pasture[muniscsv$FR=="FR6"]<-1
muniscsv$Other<-0
muniscsv$Other[muniscsv$FR=="FR7"]<-1
muniscsv$"Land Protection"<-1
#muniscsv$`Land Price`<-signif(muniscsv$`Land Price`, digits=3)
muniscsv$`Land Price`<-1500-muniscsv$`Land Price`
muniscsv$`Land Price`<-muniscsv$`Land Price`/1500
muniscsv$Ports<-NULL
muniscsv$Soil<-NULL
muniscsv$Slope<-NULL
muniscsv$Landuse<-NULL
names(statecsv)[5]="muniID"
bzro<-inner_join(statecsv, muniscsv, by = "muniID")
statedata<-bzro[, c(2,5)]
statedata$DoubleCropping<-0
statedata$DoubleCropping[statedata$CD_GCUF==50]<-1
statedata$DoubleCropping[statedata$CD_GCUF==51]<-1
statedata$DoubleCropping[statedata$CD_GCUF==41]<-1
statedata$DoubleCropping[statedata$CD_GCUF==52]<-1
statedata$DoubleCropping[statedata$CD_GCUF==31]<-1
statedata$DoubleCropping[statedata$CD_GCUF==35]<-1
statedata$CD_GCUF<-NULL
statedata<-statedata[!duplicated(statedata$muniID),]
muniscsv<-inner_join(muniscsv, statedata, by = "muniID")

names(muniscsv)[1]=""
muniscsv$`Growing Season`[muniscsv$DoubleCropping==1]<-1
muniscsv$DoubleCropping<-NULL
muniscsv$FR[(muniscsv$FR=="FR1"|muniscsv$FR=="FR2")&(muniscsv$`Growing Season`==1)]<-"FR3"
muniscsv$FR[(muniscsv$FR=="FR3")&(muniscsv$`Growing Season`==0)]<-sample(1:2, nrow(muniscsv), replace=T)
muniscsv$FR[muniscsv$FR==2]<-"FR2"
muniscsv$FR[muniscsv$FR==1]<-"FR1"
write.csv(muniscsv,"region.csv", row.names = F)



