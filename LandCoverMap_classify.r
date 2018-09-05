#classify land cover map

rm(list=ls())
library(raster)
library(readxl)

##Load Data

#unzip if needed. otherwise assumes original land cover maps are in Data/ASCII 
unzip(zipfile="Data/MapBiomas_23_ASCII_unclassified_allYears.zip",exdir="Data")  # unzip file 

#Classification from Excel
classification <- read_excel("Data/MapBiomas_CRAFTY_classifications.xlsx", sheet = "PastureB", range="B2:C21", col_names=F)  
cname <- "PastureB"

years <- seq(2000,2001,1)

for(year in years)
{
  map <- raster(paste0("Data/ASCII/brazillc_",year,"_5km_int.txt"))
  map <- reclassify(map, rcl=as.matrix(classification))
  writeRaster(map, paste0("Data/LandCover",year,"_",cname,".asc"), format = 'ascii', overwrite=T)
}

unlink("Data/ASCII", recursive = T) #delete ASCII directory created above
