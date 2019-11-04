#script to create 'access' capital maps

#requires maps output from reateSingleLCMap.r

library(raster)

#assumes focal value = 1
bkg <- 0   #background value 
buf <- 0.5  #buffer value
buf_width <- 5000  #width of buffer in m
outname <- "NatureAccess2001"

BKGs <- list(0,0.5,0.5)
BUFs <- list(0.5,0.75,0.75)
LCs <- list("Nature", "Agri", "OAgri")


for(i in seq_along(LCs)){
  
  print(LCs[[i]])
  print(paste0("Start: ",Sys.time()))

  #create single LC map for the appropriate LC createSingleLCMap.r
  lc <- raster(paste0("Data/ObservedLCmaps/singleLC_",LCs[[i]],"_2001_PastureB_Disagg.asc"))  #land cover from LandCoverMap.r (or ClassifyDisaggregateMap.r)
  
  crs(lc) <- latlong <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
  
  #crop (for testing)
  #e <- extent(-60,-55,-15,-10)
  #lc <- crop(lc,e)
  #plot(lc)
  
  lc[lc == 0] <- NA  #set 0 to NA for buffer
  
  bf <- buffer(lc, width=buf_width, doEdge=T)  #buffer width of 5km
  
  s <- stack(lc, bf)
  
  s[is.na(s)] <- BKGs[[i]]
  #plot(s)
  
  setB <- function(a , b){
    ifelse(a != 1 & b == 1, BUFs[[i]], a)
  }
  
  out <- overlay(s, fun=setB)
  
  writeRaster(out, paste0("Data/",LCs[[i]],"Access.asc"), format = 'ascii', overwrite=T)
  
  print(paste0("End: ",Sys.time()))
}
