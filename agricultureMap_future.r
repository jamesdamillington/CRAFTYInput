#script to create agriculture capital map for future climate scenarios
#requires maps from soilMap.r, slopeMap.r and climate data from CORDEX project via via CEDA: https://esgf-index1.ceda.ac.uk/search/cordex-ceda/
#soil and slope are assumed not to change through time

#To prepare this map of land use capacity we used two key variables that influences large-scale annual crop production in Brazil.
#Variable 1: Slope
#Variable 2: Climate (using method described in Victoria et al. 2007 DOI: 10.1175/EI198.1)
#These are combined into a final Agriculture Capital. 


rm(list = ls())
library(raster)
library(tidyverse)
library(ncdf4)

#directory in which to save the data
outputDir <- "Data/FutureClimate/agriCapital/"

#read munis.r as latlong
#unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data")  #unzip
munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")
latlong <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
crs(munis.r) <- latlong

#Variable 1: Slope
#In Brazil up to 90% of the agricultural areas of annual crops, such as soybean and maize, are in slopes less than 5%. 
#According to Pereira and Lombardi (2004), mechanization limitation due to slope, in an area without rocks or stones issues, are as follows:
#	0 – (0 – 3% No limitation)
#	1 – (3 – 8% Slight limitation )
#	2 – (8 – 13% Moderate limitation)
#	3 – (13 – 20% Strong limitation)
#	4* – (20 – 45 % Very strong limitation)
#	4* – (>45% Unsuitable)
#NB: *We use the code 4 for both slope classes because for the purposes of soybean/maize production, both are unsuitable.

#the slope map with the above classes is created in slopeMAp.r

#read soil texture and set bucket size (see email from Daniel Victoria 2017-11-21)
#unzip(zipfile="Data/vslope_2018-04-30.zip",exdir="Data")  #unzip
vslope.m<-raster("Data/vslope_2018-04-30.asc")  



#Variable 2: Climate
#Climate has an importance in all stages of the plant growth, from the planting season to harvest. 
#To understand climatic limitations associated with plant growth and agricultural production, we used the dryness index, which describe the relation between water deficit and potential evapotranspiration (Pereira and Lombardi, 2004), both obtained from the Thornthwaite – Matter climatic water balance. 
#This index express the water deficit in percentage of potential evapotranspiration and is calculated by the equation:
#Di = 100 DEF / PET
#where Di (%) is the dryness index; DEF represents the water deficit; and PET the potential evapotranspiration. 
#In addition, the dryness index was  combined with the number of months where the  water deficit was greater than 5 mm (Pereira and Lombardi, 2004). 

#To cultivate at least one crop year-round such as soybean or maize, the region of production needs around 4 to 5 months of rainfall and the distribution and amount of available water is key factor. 
#Therefore, the proposed method use the Table 1 to calculate per pixel, the combination of Di with the length of the dry season. 
#For these calculations, the Worldclim monthly climatic dataset (30 year monthly mean) was used.


#soil is used to calculate plant available water
#read soil texture and set bucket size (see email from Daniel Victoria 2017-11-21)
#unzip(zipfile="Data/soilT_2018-05-01.zip",exdir="Data")  #unzip
soil<-raster("Data/soilT_2018-05-01.asc")  

#PAW is Plant Available Water
PAW<-soil
PAW[PAW==1]<-0
PAW[PAW==2]<-0
PAW[PAW==3]<-75
PAW[PAW==4]<-55
PAW[PAW==5]<-35


nc2raster_f <- function(ncname, ncyear, ncvar)
{
  #this is a modified version of nc2raster function found in agricultureMap.r
  #returns a raster stack of 12 layers for a single year from CORDEX data
  
  #ncname is a character string of the ncdf file (constructed using fn_fromRcpYearVar function)
  #ncyear is an integer indicating which year from the ncdf file is to be returned (usually constructed using y_fromYear function)
  #ncvar is a character string indicating which variable from the nc file to access (i.e. "pr", "tas", "tasmax")
  
  nc <- nc_open(ncname)
  pre_array <- ncvar_get(nc,ncvar)
  lon <- ncvar_get(nc,"lon")
  lat <- ncvar_get(nc,"lat")
  
  M <- length(lon)
  N <- length(lat)
  dx <- diff(lon[1:2])
  dy <- diff(lat[1:2])
  
  startmonth <- (ncyear * 12) - 11
  
  s1 <- flip(raster(t(pre_array[,,startmonth]), xmn=lon[1]-dx/2, xmx=lon[M]+dx/2, ymn=lat[1]-dy/2, ymx=lat[N]+dy/2, crs=CRS("+init=epsg:4326")), direction='y')
  
  startmonth <- startmonth + 1
  endmonth <- startmonth + 10 
  
  for(mon in startmonth:endmonth)
  {
    s1 <- stack(s1, flip(raster(t(pre_array[,,mon]), xmn=lon[1]-dx/2, xmx=lon[M]+dx/2, ymn=lat[1]-dy/2, ymx=lat[N]+dy/2, crs=CRS("+init=epsg:4326")), direction='y'))
  }
  
  names(s1) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  return(s1)
}




#function to return final digit from a four-digit integer (for use in nc2raster)
y_fromYear <- function(year)
{
  y <- year %% 10
  if(y == 0) { y <- 10 }
  return(y)
}


#function to set climate file name (for use in nc2raster_f) from 
#rcp value (rcp2.6 = 26, rcp4.5 = 45, rcp8.5 = 85), 
#year integer and 
#variable (pr, tasmin, tasmax)
fn_fromRcpYearVar <- function(rcp, year, var)
{
  directory = paste0("Data/FutureClimate/",var,"/HADGEM2-rcp",rcp,"/")
  fn1 = paste0("_SAM-44_MOHC-HadGEM2-ES_rcp",rcp,"_r1i1p1_SMHI-RCA4_v3_mon_")
  
  yr = "201101-202012"
  if(year > 2020 & year <= 2030) { yr = "202101-203012"}
  if(year > 2030 & year <= 2040) { yr = "203101-204012"}
  if(year > 2040 & year <= 2050) { yr = "204101-205012"}
  
  return(paste0(directory, var, fn1, yr,".nc"))
}



#extent object to use in pdf map plots
BRA.ext <- extent(-62.39713, -35.43949, -33.89756, -4.06125)

#coords of above found using
#plot(munis.r)
#drawExtent()


calcAgriMaps <- function(munis.r, PAW, year, BRA.e, RCP, writeClimRast, writeClimPdf)
{
  #generate timeRange and filenames for this year
  tr <- y_fromYear(year)
  prefn <- fn_fromRcpYearVar(RCP,year,"pr")      # variable_units = kg m-2 s-1, time_frequency = mon, variable_long_name = Precipitation, 
  tmnfn <- fn_fromRcpYearVar(RCP,year,"tasmin")  # variable_units = K, time_frequency = mon, variable_long_name = Daily Minimum Near-Surface Air Temperature 
  tmxfn <- fn_fromRcpYearVar(RCP,year,"tasmax")  # variable_units = K, time_frequency = mon, variable_long_name = Daily Maximum Near-Surface Air Temperature  
  
  #read climate files
  pre <- nc2raster_f(ncname=prefn,ncyear=tr,ncvar="pr")
  tmn <- nc2raster_f(ncname=tmnfn,ncyear=tr,ncvar="tasmin")
  tmx <- nc2raster_f(ncname=tmxfn,ncyear=tr,ncvar="tasmax")
  
  #project and crop bricks to extent we want for Brazil
  pre.b <- projectRaster(pre, munis.r)
  pre.b <- mask(x=pre.b, mask=munis.r)
  
  tmn.b <- projectRaster(tmn, munis.r)
  tmn.b <- mask(x=tmn.b, mask=munis.r)
  
  tmx.b <- projectRaster(tmx, munis.r)
  tmx.b <- mask(x=tmx.b, mask=munis.r)
  
  #convert units here (smaller raster tnan original data)
  mon_secs <- 3600 * 24 * 30  #seconds per month
  pre.b <- pre.b * mon_secs #convert kg m-1 s-1 to mm month-1 (kg m-1 is equivalent to mm so multiply by number of seconds to get mm per month)
  tmn.b <- tmn.b -272.15  #convert K to degC
  tmx.b <- tmx.b -272.15  #convert K to degC
  
  #caclulate average temperature by month (brick)
  avtemp.b <- 0.36*(3*tmx.b-tmn.b)
  
  #annual temp raster layer
  Ta<-mean(avtemp.b)
  
  #set params needed to calculate PET
  Days <- list(31,28,31,30,31,30,31,31,30,31,30,31) #list of days in the month 
  Idex <- 12*((0.2*Ta)^1.514)#1st regional thermal index
  Adex <- 0.49239+1.7912*10^-2*Idex-7.71*10^-5*Idex^2+6.75*10^-7*Idex^3#2nd regional thermal index
  Ndex <- 12##NEED REAL VALUE
  
  
  #initialize PET with mean temperatures
  PET.b <- avtemp.b 
  
  #function to calculate Potential Evapotranspiration (PET)
  calcPET <- function(PET, D, I, a, N)
  {
    #if mean temperature <= 0
    PET[PET<=0]<-0
    
    #if mean temperature > 26.5
    PET[PET>26.5]<-(-415.85+32.24*PET[PET>26.5]-0.43*(PET[PET>26.5]^2))*(N/12)*(D/30)
    
    #else
    PET[PET>0&PET<=26.5]<-0.0444*((10*(PET[PET>0&PET<=26.5]/I[PET[]>0&PET[]<=26.5]))^a[PET[]>0&PET[]<=26.5])*N*D
    
    return(PET)
  }
  
  
  #map2 to loop over raster brick (layer per month) and Days list (from purrr, see http://r4ds.had.co.nz/iteration.html)
  #brick needs to passed as a list (see https://geocompr.robinlovelace.net/location.html)
  PET.b <- 
    map2(as.list(PET.b), Days, calcPET, I = Idex, a = Adex, N = Ndex) %>% 
    stack()  #remember to re-stack the list after function
  
  names(PET.b) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
 
  #see Victoria et al. 2007 DOI: 10.1175/EI198.1 Table 2 for equations
  #initialise water storage variables 
  
  Stoi <- PAW  #Stoii is month i-1 storage
  Stoii <- Stoi #Stoi is month i storage (in first month use same values)

  allmeanStoi <- vector("double", 12)  #vector to hold meanStoi for each month
  
  #for creating empty rasters and bricks
  nullRaster <- munis.r
  nullRaster[!is.na(nullRaster)] <- 0  #set anywhere that is not 'no data' in munis.r to 0

  DEF.b <- stack(replicate(12, nullRaster)) #empty brick to save all month's DEF
  ET.b <- stack(replicate(12, nullRaster))  #empty brick to save all month's ET
  
  DEF <- nullRaster #empty layer for temp useage in loop
  ET <- nullRaster #empty layer for temp useage in loop
  
  #par(mfrow=c(1,1))
  
  #see loopProofs (need to use loop, cannot use map)
  for(i in 1:12)
  {
    #hold current values of Stoi to set Stoii for next month below (this is why we can't use map)
    tempStoi <- Stoi
    
    P <- pre.b[[i]]    #get this month's precipitation (for clarity in equations below)
    PET <- PET.b[[i]]  #get this month's PET (for clarity in equations below)
    
    #if pptn < PET set storage
    Stoi[P<PET] <- Stoii[P<PET] * exp(P[P<PET] - PET[P<PET]/PAW[P<PET])
    
    #if pptn >= PET set storage
    Stoi[P>=PET] <- Stoii[P>=PET] + (P[P>=PET] - PET[P>=PET])
    
    #update Stoii ready for next month
    Stoii<-tempStoi
    
    #where Sto > PAW
    Stoi[Stoi[]>PAW[]] <- PAW[Stoi[]>PAW[]]
    
    #save mean Stoi value for this month
    allmeanStoi[i] <- cellStats(Stoi, "mean")
    
    #calculate delta storage
    trSto <- Stoi - Stoii

    #reset ET for this loop
    ET <- nullRaster
    
    #where pptn < PET
    ET[P<PET] <- P[P<PET] - trSto[P<PET]
    
    #where P >= PET
    ET[P>=PET] <- PET[P>=PET]
    
    #reset DEF for this loop 
    DEF <- nullRaster
    
    #where pptn < PET
    DEF[P<PET] <- PET[P<PET] - ET[P<PET]
    
    #where P >= PET
    DEF[P>=PET]<-0
    
    #copy DEF to DEF brick
    DEF.b[[i]] <- DEF
    ET.b[[i]] <- ET
  
  }
  
  names(DEF.b) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  names(ET.b) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

  #calculate Dryness Index
  avDEF<-mean(DEF.b)#mean annual DEF
  avPET<-mean(PET.b)#mean annual PET
  Di <- (100*avDEF) / avPET
  
  
  #write data to files
  if(writeClimRast)
  {
    writeRaster(avDEF, paste0(outputDir,"DEF_rcp",RCP,"_",year,".asc"), format = 'ascii', overwrite=T)
    writeRaster(avPET, paste0(outputDir,"PET_rcp",RCP,"_",year,".asc"), format = 'ascii', overwrite=T)
  }
  
  if(writeClimPdf)
  {
    pdf(paste0(outputDir,"Temperature_rcp",RCP,"_",year,".pdf"))
    plot(avtemp.b, ext = BRA.e)
    dev.off()
    
    pdf(paste0(outputDir,"Precip_rcp",RCP,"_",year,".pdf"))
    plot(pre.b, ext = BRA.e)
    dev.off()
    
    pdf(paste0(outputDir,"DEF_rcp",RCP,"_",year,".pdf"))
    plot(DEF.b, ext = BRA.e)
    dev.off()
    
    pdf(paste0(outputDir,"PET_rcp",RCP,"_",year,".pdf"))
    plot(PET.b, ext = BRA.e)
    dev.off()

    pdf(paste0(outputDir,"ET_rcp",RCP,"_",year,".pdf"))
    plot(ET.b, ext = BRA.e)
    dev.off()
    
    pdf(paste0(outputDir,"meanDEF_rcp",RCP,"_",year,".pdf"))
    plot(avDEF, ext = BRA.e)
    dev.off()
    
    pdf(paste0(outputDir,"meanPET_rcp",RCP,"_",year,".pdf"))
    plot(avPET, ext = BRA.e)
    dev.off()
  }
  
  
  
  #Number of months with water deficit
  #helper function
  countZero <- function(vect)
  {
    return(sum(vect == 0))
  }
  
  DEFmonths <- calc(DEF.b, countZero)
  Stoidiffc <- allmeanStoi[12] - allmeanStoi[1]
  #Stoidiffc

  
  
  #Classify climate limitation (combination of deficit months and dryness index)
  #Limitation degree:
    #0 – Null, no limitation climatic restrictions
    #1 – Slight 
    #2 – Moderate
    #3 – Strong
    #4 – Very strong

  #Agri1<-Di
  Agri1 <- nullRaster
  
  Agri1[DEFmonths[]==0&Di[]<20]<-0
  Agri1[DEFmonths[]==0&Di[]>=20&Di[]<40]<-0
  Agri1[DEFmonths[]==0&Di[]>=40&Di[]<60]<-1
  Agri1[DEFmonths[]==0&Di[]>=60&Di[]<80]<-2
  Agri1[DEFmonths[]==0&Di[]>=80]<-3
  
  Agri1[DEFmonths[]>=1&DEFmonths[]<=2&Di[]<20]<-0
  Agri1[DEFmonths[]>=1&DEFmonths[]<=2&Di[]>=20&Di[]<40]<-1
  Agri1[DEFmonths[]>=1&DEFmonths[]<=2&Di[]>=40&Di[]<60]<-2
  Agri1[DEFmonths[]>=1&DEFmonths[]<=2&Di[]>=60&Di[]<80]<-3
  Agri1[DEFmonths[]>=1&DEFmonths[]<=2&Di[]>=80]<-4
  
  Agri1[DEFmonths[]>=3&DEFmonths[]<=5&Di[]<20]<-1
  Agri1[DEFmonths[]>=3&DEFmonths[]<=5&Di[]>=20&Di[]<40]<-2
  Agri1[DEFmonths[]>=3&DEFmonths[]<=5&Di[]>=40&Di[]<60]<-3
  Agri1[DEFmonths[]>=3&DEFmonths[]<=5&Di[]>=60&Di[]<80]<-4
  Agri1[DEFmonths[]>=3&DEFmonths[]<=5&Di[]>=80]<-4
  
  Agri1[DEFmonths[]>=6&DEFmonths[]<=8&Di[]<20]<-2
  Agri1[DEFmonths[]>=6&DEFmonths[]<=8&Di[]>=20&Di[]<40]<-3
  Agri1[DEFmonths[]>=6&DEFmonths[]<=8&Di[]>=40&Di[]<60]<-4
  Agri1[DEFmonths[]>=6&DEFmonths[]<=8&Di[]>=60&Di[]<80]<-4
  Agri1[DEFmonths[]>=6&DEFmonths[]<=8&Di[]>=80]<-4
  
  Agri1[DEFmonths[]>=9&DEFmonths[]<=12&Di[]<20]<-3
  Agri1[DEFmonths[]>=9&DEFmonths[]<=12&Di[]>=20&Di[]<40]<-4
  Agri1[DEFmonths[]>=9&DEFmonths[]<=12&Di[]>=40&Di[]<60]<-4
  Agri1[DEFmonths[]>=9&DEFmonths[]<=12&Di[]>=60&Di[]<80]<-4
  Agri1[DEFmonths[]>=9&DEFmonths[]<=12&Di[]>=80]<-4
  
  #par(mfrow=c(1,1))
  #plot(Agri1)
  
  writeRaster(Agri1, paste0(outputDir,"agriClimateSlope_rcp",RCP,"_",year,".asc"), format = 'ascii', overwrite=T)
  
  pdf(paste0(outputDir,"agriClimateSlope_rcp",RCP,"_",year,".pdf"))
  plot(Agri1, ext = BRA.e)
  dev.off()
  
  
  #create map of double cropping
  #discussion with Ramon by email in March 2019 suggested that DC is not possible on limitation 3 and 4
  dc.f <- Agri1
  values(dc.f)[values(dc.f)==0 | values(dc.f)==2] = 1 
  values(dc.f)[values(dc.f)==3 | values(dc.f)==4] = 0 
  writeRaster(dc.f, paste0(outputDir,"DC_rcp",RCP,"_",year,".asc"), format = 'ascii', overwrite=T)
  
  pdf(paste0(outputDir,"DC_rcp",RCP,"_",year,".pdf"))
  plot(dc.f, ext = BRA.e)
  dev.off()
 
  
  #Considering the two variables, we join by combining the 6 classes of slope with the 5 classes of climate, resulting in 30 combinations grouped according to the table (below).
  #Each combination is reclassified with a code accordingly to the maximum value of the combination (the value of one of the two variables at least). 
  #The code (suitability classes which equate to Agricultural Capital for CRAFTY) for the combined map was divided by 4 to represent the scale values from 0 to 1.

  vagri.f <- Agri1
  values(vagri.f)[values(Agri1)==0&values(vslope.m)==0] = 1      #best
  values(vagri.f)[values(Agri1)==0&values(vslope.m)==1] = 0.75
  values(vagri.f)[values(Agri1)==0&values(vslope.m)==2] = 0.5
  values(vagri.f)[values(Agri1)==0&values(vslope.m)==3] = 0.25
  values(vagri.f)[values(Agri1)==0&values(vslope.m)==4] = 0.1
  
  values(vagri.f)[values(Agri1)==1&values(vslope.m)==0] = 0.75
  values(vagri.f)[values(Agri1)==1&values(vslope.m)==1] = 0.75
  values(vagri.f)[values(Agri1)==1&values(vslope.m)==2] = 0.5
  values(vagri.f)[values(Agri1)==1&values(vslope.m)==3] = 0.25
  values(vagri.f)[values(Agri1)==1&values(vslope.m)==4] = 0.1
  
  values(vagri.f)[values(Agri1)==2&values(vslope.m)==0] = 0.5
  values(vagri.f)[values(Agri1)==2&values(vslope.m)==1] = 0.5
  values(vagri.f)[values(Agri1)==2&values(vslope.m)==2] = 0.5
  values(vagri.f)[values(Agri1)==2&values(vslope.m)==3] = 0.25
  values(vagri.f)[values(Agri1)==2&values(vslope.m)==4] = 0.1
  
  values(vagri.f)[values(Agri1)==3&values(vslope.m)==0] = 0.25
  values(vagri.f)[values(Agri1)==3&values(vslope.m)==1] = 0.25
  values(vagri.f)[values(Agri1)==3&values(vslope.m)==2] = 0.25
  values(vagri.f)[values(Agri1)==3&values(vslope.m)==3] = 0.25
  values(vagri.f)[values(Agri1)==3&values(vslope.m)==4] = 0.1
  
  values(vagri.f)[values(Agri1)==4&values(vslope.m)==0] = 0.1
  values(vagri.f)[values(Agri1)==4&values(vslope.m)==1] = 0.1
  values(vagri.f)[values(Agri1)==4&values(vslope.m)==2] = 0.1
  values(vagri.f)[values(Agri1)==4&values(vslope.m)==3] = 0.1
  values(vagri.f)[values(Agri1)==4&values(vslope.m)==4] = 0.1
  values(vagri.f)[values(vagri.f)>1]=0                              #worst

  #!check does this need to be resampled/masked before writing?!
  writeRaster(vagri.f, paste0(outputDir,"agricultureCapital_rcp",RCP,"_",year,".asc"), format = 'ascii', overwrite=T)
  
  pdf(paste0(outputDir,"agricultureCapital_rcp",RCP,"_",year,".pdf"))
  plot(vagri.f, ext = BRA.e)
  dev.off()
  
  rm(pre,tmn,tmx,pre.b,tmn.b,tmx.b,PET.b,DEF.b,ET.b,Agri1,vagri.f)
  
}

#loop over rcps
for(x in c(26, 45)){
  #loop over years
  for(y in 2015:2030)
  {
    calcAgriMaps(munis.r, PAW, y, BRA.ext, RCP=x, writeClimRast=F, writeClimPdf=T)
    print(paste0("rcp", x, ", ", y," done"))
  }
}


