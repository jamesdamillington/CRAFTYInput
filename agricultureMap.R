#JM editing code by Val K (2018-05-01)

rm(list = ls())
#dpath <- "C:/Users/k1076631/CRAFTYinput"
#setwd(dpath)

library(cruts)    #or see ncdf4 http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm
library(raster)
library(tidyverse)
library(ncdf4)

#unzip cruts files (if needed)
unzip(zipfile="Data/cru_ts4.zip",exdir="Data/cruts")  # unzip all files 

#read munis.r as latlong
unzip(zipfile="Data/sim10_BRmunis_latlon_5km_2018-04-27.zip",exdir="Data")  #unzip
munis.r <- raster("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")
latlong <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
crs(munis.r) <- latlong

#read soil and set bucket size (see email from Daniel Victoria 2017-11-21)
unzip(zipfile="Data/soilT_2018-05-01.zip",exdir="Data")  #unzip
soil<-raster("Data/soilT_2018-05-01.asc")  

PAW<-soil
PAW[PAW==1]<-0
PAW[PAW==2]<-0
PAW[PAW==3]<-75
PAW[PAW==4]<-55
PAW[PAW==5]<-35
#plot(PAW)

nc2raster <- function(ncname, ncyear, ncvar)
{
  #this is hacked version of cruts2raster function in library(cruts) see https://rdrr.io/cran/cruts/src/R/import-export.R
  #returns a raster stack of 12 layers for a single year from cru_ts data (e.g. http://data.ceda.ac.uk//badc/cru/data/cru_ts/cru_ts_4.01/data/)
  #needed becase cruts2raster returns same data (year) regardless of timeRange provided 
  
  #ncname is a character string of the ncdf file (e.g."cru_ts4.01.2001.2010.pre.dat.nc")
  #ncyear is an integer indicating which year from the ncdf file is to be returned (so for above file ncyear <- 1 would return data for 2001, ncyear <- 4 would give 2004 etc)
  #ncvar is a character string indicating which variable from the nc file to access (e.g. "pre", "tmp")
  
  nc <- nc_open(ncname)
  pre_array <- ncvar_get(nc,ncvar)
  lon <- ncvar_get(nc,"lon")
  lat <- ncvar_get(nc,"lat")
  
  M <- length(lon)
  N <- length(lat)
  dx <- diff(lon[1:2])
  dy <- diff(lat[1:2])
  
  startmonth <- (ncyear * 12) - 11
  
  s1 <- raster(t(pre_array[,,startmonth][,N:1]), xmn=lon[1]-dx/2, xmx=lon[M]+dx/2, ymn=lat[1]-dy/2, ymx=lat[N]+dy/2, crs=CRS("+init=epsg:4326"))
  
  startmonth <- startmonth + 1
  endmonth <- startmonth + 10 #this should be 10?
  
  for(mon in startmonth:endmonth)
  {
    s1 <- stack(s1, raster(t(pre_array[,,mon][,N:1]), xmn=lon[1]-dx/2, xmx=lon[M]+dx/2, ymn=lat[1]-dy/2, ymx=lat[N]+dy/2, crs=CRS("+init=epsg:4326")))
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


#function to set climate file name (for use in nc2raster) from a year integer and variable (pre, tmn, tmx)
fn_fromYearVar <- function(year, var)
{
  yr = "Data/cru_ts4.01.1991.2000."
  if(year > 2000 & year <= 2010) { yr = "cru_ts4.01.2001.2010."}
  if(year > 2010 & year <= 2016) { yr = "cru_ts4.01.2011.2016."}
  
  return(paste0(yr,var,".dat.nc"))
}



#extent object to use in pdf map plots
BRA.ext <- extent(-62.39713, -35.43949, -33.89756, -4.06125)

#coords of above found using
#plot(munis.r)
#drawExtent()

for(y in 1999:2015)
{
  calcAgriMaps(munis.r, PAW, y, BRA.ext)
  print(paste0(y," done"))
}


calcAgriMaps <- function(munis.r, PAW, year, BRA.e)
{
  #generate timeRange and filenames for this year
  tr <- y_fromYear(year)
  prefn <- fn_fromYearVar(year,"pre")
  tmnfn <- fn_fromYearVar(year,"tmn")
  tmxfn <- fn_fromYearVar(year,"tmx")
  
  print(tr)
  print(prefn)
  print(tmnfn)
  print(tmxfn)
  
  #read climate files
  pre <- nc2raster(ncname=prefn,ncyear=tr,ncvar="pre")
  tmn <- nc2raster(ncname=tmnfn,ncyear=tr,ncvar="tmn")
  tmx <- nc2raster(ncname=tmxfn,ncyear=tr,ncvar="tmx")
  
  pdf(paste0("Data/pre",year,".pdf"))
  plot(pre, ext = BRA.e)
  dev.off()
  
  #project and crop bricks to extent we want for Brazil
  pre.b <- projectRaster(pre, munis.r)
  pre.b <- mask(x=pre.b, mask=munis.r)
  
  tmn.b <- projectRaster(tmn, munis.r)
  tmn.b <- mask(x=tmn.b, mask=munis.r)
  
  tmx.b <- projectRaster(tmx, munis.r)
  tmx.b <- mask(x=tmx.b, mask=munis.r)
  
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
  
  #check
  pdf(paste0("Data/PET",year,".pdf"))
  plot(PET.b, ext = BRA.e)
  dev.off()
  
  #see Victoria et al. 2007 DOI: 10.1175/EI198.1 Table 2 for equations
  #initialise water storage variables 
  
  Stoi <- PAW  #Stoii is month i-1 storage
  Stoii <- Stoi #Stoi is month i storage (in first month use same values)
  
  #next two not needed in loop?
  #meanStoi <- cellStats(Stoi, "mean")  #holds the mean storage for month i
  #meanStoii<- cellStats(Stoii, "mean") #holds the mean storage for month i - 1
  
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
    
    #par(mfrow=c(3,3))
    
    #plot(P, main = paste0("P, ", i))
    #plot(PET, main = paste0("PET, ", i))
    #plot(Stoi, main = paste0("Stoi, ", i, " (pre)"))
    
    #if pptn < PET set storage
    #old: Stoi[prejancut[]>=PET[]]<-Stoii[prejancut[]>=PET[]]+(prejancut[prejancut[]>=PET[]]-PET[prejancut[]>=PET[]])
    Stoi[P<PET] <- Stoii[P<PET] * exp(P[P<PET] - PET[P<PET]/PAW[P<PET])
    
    #if pptn >= PET set storage
    #old: Stoi[prejancut[]>=PET[]]<-Stoii[prejancut[]>=PET[]]+(prejancut[prejancut[]>=PET[]]-PET[prejancut[]>=PET[]])
    Stoi[P>=PET] <- Stoii[P>=PET] + (P[P>=PET] - PET[P>=PET])
    
    #plot(Stoi, main = paste0("Stoi, ", i, " (mid)"))
    
    #update Stoii ready for next month
    Stoii<-tempStoi
    
    #where Sto > PAW
    Stoi[Stoi[]>PAW[]] <- PAW[Stoi[]>PAW[]]
    #plot(Stoi, main = paste0("Stoi, ", i, " (post)"))
    
    #save mean Stoi value for this month
    allmeanStoi[i] <- cellStats(Stoi, "mean")
    
    #plot(Stoii, main = paste0("Stoii, ", i))
    #calculate delta storage
    trSto <- Stoi - Stoii
    #plot(trSto, main = paste0("trSto, ", i))
    
    #reset ET for this loop
    ET <- nullRaster
    
    #where pptn < PET
    #old: ET[prejancut[]<PET[]]<-prejancut[prejancut[]<PET[]]-trSto[prejancut[]<PET[]]
    ET[P<PET] <- P[P<PET] - trSto[P<PET]
    
    #where P >= PET
    #old: ET[prejancut[]>=PET[]]<-PET[prejancut[]>=PET[]]
    ET[P>=PET] <- PET[P>=PET]
    
    #plot(ET, main = paste0("ET, ", i))
    
    #reset DEF for this loop 
    DEF <- nullRaster
    
    #where pptn < PET
    #old: DEF[prejancut[]<PET[]]<-PET[prejancut[]<PET[]]-ET[prejancut[]<PET[]]
    DEF[P<PET] <- PET[P<PET] - ET[P<PET]
    
    #where P >= PET
    DEF[P>=PET]<-0
    
    #plot(DEF, main = paste0("DEF, ", i))
    
    #copy DEF to DEF brick
    DEF.b[[i]] <- DEF
    ET.b[[i]] <- ET
  
  }
  
  #par(mfrow=c(1,1))
  
  names(DEF.b) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  names(ET.b) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  
  
  
  avDEF<-mean(DEF.b)#mean annual DEF
  writeRaster(avDEF, paste0("Data/DEF",year,".asc"), format = 'ascii', overwrite=T)
  
  avPET<-mean(PET.b)#mean annual PET
  writeRaster(avPET, paste0("Data/PET",year,".asc"), format = 'ascii', overwrite=T)
  
  pdf(paste0("Data/DEF",year,".pdf"))
  plot(DEF.b, ext = BRA.e)
  dev.off()
  
  pdf(paste0("Data/ET",year,".pdf"))
  plot(ET.b, ext = BRA.e)
  dev.off()
  
  pdf(paste0("Data/meanDEF",year,".pdf"))
  plot(avDEF, ext = BRA.e)
  dev.off()
  
  pdf(paste0("Data/meanPET",year,".pdf"))
  plot(avPET, ext = BRA.e)
  dev.off()
  
  #Monthsdef[DEFjan[]==0]<-Monthsdef[DEFjan[]==0]+1
  #Stoidif<-meanStoi-meanStoi1999jan
  #################################
  
  #v <- as.vector(rbinom(10,1,0.5))
  
  countZero <- function(vect)
  {
    return(sum(vect == 0))
  }
  
  #countZero(v)
  
  
  DEFmonths <- calc(DEF.b, countZero)
  Stoidiffc <- allmeanStoi[12] - allmeanStoi[1]
  Stoidiffc

  
  #PETstack<-stack(PETjan,PETfeb,PETmar,PETapr,PETmay,PETjun,PETjul,PETaug,PETsep,PEToct,PETnov,PETdec)
  #PET<-mean(PETstack)#mean annual PET
  
  #DEFstack<-stack(DEFjan,DEFfeb,DEFmar,DEFapr,DEFmay,DEFjun,DEFjul,DEFaug,DEFsep,DEFoct,DEFnov,DEFdec)
  #DEF<-mean(DEFstack)#mean annual DEF
  
  Di <- (100*avDEF) / avPET
  
  Agri1<-Di
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
  
  par(mfrow=c(1,1))
  plot(Agri1)
  
  writeRaster(Agri1, paste0("Data/agriculture",year,".asc"), format = 'ascii', overwrite=T)
  
  pdf(paste0("Data/agriculture",year,".pdf"))
  plot(Agri1, ext = BRA.e)
  dev.off()
  
  rm(pre,tmn,tmx,pre.b,tmn.b,tmx.b,PET.b,DEF.b,ET.b,Agri1)
}

unlink("Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")
unlink("Data/soilT_2018-05-01.asc")
unlink("Data/cruts", recursive = T)
