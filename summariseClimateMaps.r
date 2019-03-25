#summarises annual climate maps (produced by agricultureMap_future.r or agricultureMap.r)
#mean, median, max, min, sd, q5, q95 of DEF, PET Temp, pptn, Di by RCP and Year

rm(list = ls())
library(raster)

#function to clip raster to specified states (specified in states_ls list)
clipStates <- function(inRaster, statemap, states_ls)
{
  #substitute values so that only specified states have data
  #first create the data frame
  mdf <- data.frame()
  for(m in seq_along(states_ls)) {
    if(m == 1) { 
      mdf <- data.frame(a=states_ls[m], b=1) 
    }  else  {  
      mdf <- rbind(mdf, c(states_ls[m], 1))  
    }
  }
  #now subs in the statemap 
  statemap <- subs(x=statemap, y=mdf, by=1, which=2, subsWithNA=T)
  
  #trim so that statemap is extent of masked data
  smaskmap <- trim(statemap)
  
  #crop pl stack to extent of crop map 
  inRaster <- crop(inRaster, smaskmap)
  
  #mask to extent of desired states
  inRaster <- mask(inRaster, smaskmap)
  
  return(inRaster)
  
}

#directory in which to read the data
inputDir <- "Data/FutureClimate/agriCapital/"
outputFn <- "ClimateSummaryData_51.csv"

#specify states to plot (for all states provide empty list)
#states <- c()  #all states
states <- c(51) #MT

#raster of stateIDs
statemap <- raster(paste0(inputDir,"sim10_BRstates_latlon_5km.asc"))

#list of variables to summarise
Variables <- c("DEF", "PET", "Temperature", "Precipitation", "DrynessIndex")

#create output dataframe
df <- data.frame(
  Year=integer(),
  RCP=integer(),
  Variable=character(), 
  mean=double(), 
  median=double(), 
  sd=double(),
  min=double(),
  max=double(),
  q05=double(),
  q95=double()
  )

#loop over rcps
for(rcp in c(45,85)){
  #loop over years
  for(year in 2015:2017){
    
    print(paste0("rcp ", rcp, ", year ", year))
    
    #loop over variables
    for(var in Variables){
    
      #read raster
      ras <- raster(paste0(inputDir,"Annual",var,"_rcp",rcp,"_",year,".asc"))
      
      #mask if specific states are desired
      if(!is.null(states)){
        ras <- clipStates(ras, statemap, states) }
      
      #calc summary stats
      meanStat <- round(cellStats(ras, stat='mean', na.rm=T, asSample=F), 3)
      medianStat <- round(quantile(ras, probs=c(0.50), na.rm=T, names=F), 3)
      sdStat <- round(cellStats(ras, stat='sd', na.rm=T, asSample=F),4)
      minStat <- round(cellStats(ras, stat='min', na.rm=T, asSample=F), 3)
      maxStat <- round(cellStats(ras, stat='max', na.rm=T, asSample=F), 3)
      q.05 <- round(quantile(ras, probs=c(0.05), na.rm=T, names=F), 3)
      q.95 <- round(quantile(ras, probs=c(0.95), na.rm=T, names=F), 3)
      
      #bind to datafram
      yr <- data.frame(Year=year, RCP=rcp, Variable=var, mean=meanStat, median=medianStat, sd=sdStat, min=minStat, max=maxStat, q05=q.05, q95=q.95)
      df <- rbind(df, yr)
      
    }
  }
}

#write data to file (overwrites if exists)
write.csv(df, paste0(inputDir,outputFn), row.names=F, quote=F)
