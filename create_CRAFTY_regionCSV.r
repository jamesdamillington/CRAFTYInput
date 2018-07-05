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

munis.b <- raster("Data/brazillc_2000_int_reclass_5km_txt_pasture.txt")


#classify land cover map here??


#create a list of unique municipality id values
u.mids <- unique(munis.r)  #JM edit munis.r
#length(u.mids)


#read required files
#land prices from LandPriceMap.r
Lpr <- raster('LandPrice2001.asc')

#??
dPorts <- raster('d2Ports2000_2018-04-30.asc')

#classified soil map from soilMap.r
vsoil.m<-raster('vsoil_2018-05-08.asc')   

#classfied slope values from slopeMap.r 
vslope.m <- raster("Data/vslope_2018-04-30.asc")

#this is the agriculture capital map. made in agricultureMap.r 
vagri.m<-raster('Data/agricultureCapital2000.asc')   


munis.b.m<-mask(x=munis.b, mask=munis.r)   #JM edited



#plot(munis.r.latlong)
#plot(dPorts)
values(dPorts)=values(dPorts)/1000
values(dPorts)=values(dPorts)*(1/-2530)
values(dPorts)=values(dPorts)+1
values(dPorts)=round(values(dPorts), digits=3)
dPorts<- resample(dPorts, munis.r.latlong, method='bilinear')

dPorts.m <- mask(x=dPorts, mask=munis.r.latlong)
plot(dPorts.m)
#munis.r.latlong<-flip(munis.r.latlong, 'y')
munis.r.latlong<-flip(munis.r.latlong, 'y')
dPorts.m<-flip(dPorts.m, 'y')
vsoil.m<-flip(vsoil.m, 'y')
vagri.f<-flip(vagri.f, 'y')
vslope.m<-flip(vslope.m, 'y')
munis.b.m<-flip(munis.b.m, 'y')
Lpr<-flip(Lpr, 'y')
#unique values from muni id raster
u.mids <- unique(munis.r.latlong)
length(u.mids)



#create reclassify matrix: muni ids and a random value (uniform for entire muni)
set.seed(1)

#categorical
br <- cbind(u.mids, sample(c(1), length(u.mids), replace = T))#VAL only 1 behavioral type used currently
fr <- cbind(u.mids, sample(c(1,2,3,5), length(u.mids), replace = T))#VAL excluded fr4 as it shouldne be declared at initialization
agentID <- cbind(u.mids, 1:length(u.mids))#VAL attempt at unique agent ids

##choose either sample or read from file
#continuous: sampled 
# agri <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# nat <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# hum <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# dev <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# infra <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# econ <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# access <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))

#continuous: from file
#setwd(paste(belmontDataPath, "Brazil/AdminBoundaries/CRAFTYtesting", sep=""))
dat <- read.csv("createRegionCSV_dummy.csv")

#join data to u.mids in the raster (some may be missing)
df.u.mids <- as.data.frame(u.mids)
dat <- inner_join(dat, df.u.mids, by = c('CD_GCMUN' = 'u.mids'))

agri <- dat[,c('CD_GCMUN','Agriculture')]#VAL renamed to match crafty inputs
nat <- dat[,c('CD_GCMUN','Nature')]
hum <- dat[,c('CD_GCMUN','Human')]
dev <- dat[,c('CD_GCMUN','Development')]
infra <- dat[,c('CD_GCMUN','Infrastructure')]
econ <- dat[,c('CD_GCMUN','Economic')]
access <- dat[,c('CD_GCMUN','Accessibility')]


#use reclassify matrix 
br.r <- reclassify(munis.r.latlong, br)
fr.r <- reclassify(munis.r.latlong, fr)
agentID.r <- reclassify(munis.r.latlong, agentID)
agri.r <- reclassify(munis.r.latlong, agri)
nat.r <- reclassify(munis.r.latlong, nat)
hum.r <- reclassify(munis.r.latlong, hum)
dev.r <- reclassify(munis.r.latlong, dev)
infra.r <- reclassify(munis.r.latlong, infra)
econ.r <- reclassify(munis.r.latlong, econ)
access.r <- reclassify(munis.r.latlong, access)

munis.s <- stack(list(munis.r.latlong,
                      br.r,
                      fr.r,
                      agentID.r, 
                      agri.r, 
                      nat.r, 
                      hum.r, 
                      dev.r, 
                      infra.r, 
                      econ.r, 
                      access.r,
                      dPorts.m,
                      vsoil.m,
                      vslope.m,
                      vagri.f,
                      munis.b.m,
                      Lpr))

names(munis.s) <- list("muniID",
                       "BT",
                       "FR",
                       "agentid", 
                       "Agriculture",
                       "Nature",
                       "Human",
                       "Development",
                       "Infrastructure",
                       "Economic",
                       "Acessibility",
                       "Ports",
                       "Soil",
                       "Slope",
                       "Climate",
                       "Landuse",
                       "Land Price")

munis.out <- extractXYZ(munis.s)
head(munis.out)

#need to check row,col consistency with CRAFTY X,Y
write.csv(munis.out,"region.csv", row.names = F)
muniscsv <- read.csv("region.csv")
statecsv<-read.csv("Municipality_area_and_IBGE_code_number.csv")
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
muniscsv$BT<-"Cognitor"
muniscsv$agentid<-1:nrow(muniscsv)
names(muniscsv)[1]="ID"
names(muniscsv)[2]="Y"
names(muniscsv)[3]="X"
names(muniscsv)[20]="Land Price"
muniscsv$Acessibility<-(sample(90:100, size=nrow(muniscsv), replace = T))/100
muniscsv$Acessibility[muniscsv$FR=="FR5"]<-(sample(1:20, size=nrow(muniscsv), replace = T))/100
muniscsv$Infrastructure<-muniscsv$Ports
#muniscsv$Soil[muniscsv$Slope==0]<-0
#muniscsv$Slope[muniscsv$Soil==0]<-0
#muniscsv$Climate[muniscsv$Slope==0]<-0
#muniscsv$Climate[muniscsv$Soil==0]<-0
muniscsv$Agriculture<-muniscsv$Climate
muniscsv$Human<-1
muniscsv$Development<-1
muniscsv$Economic<-1
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






#code for rasterising ports and soils vector files 
