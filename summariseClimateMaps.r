#summarises annual climate maps (produced by agricultureMap_future.r or agricultureMap.r)
#mean, median, max, min, sd, q5, q95 of DEF, PET Temp, pptn, Di by RCP and Year

rm(list = ls())
library(raster)

#directory in which to read the data
inputDir <- "Data/FutureClimate/agriCapital/"
outputFn <- "ClimateSummaryData.csv"

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

#write data to file
write.csv(df, paste0(inputDir,outputFn), row.names=F, quote=F)
