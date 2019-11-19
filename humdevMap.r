
rm(list=ls())

library(raster)
library(tidyverse)
library(readxl)

######
#FUNCTIONS
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


getLCs <- function(data)
{
  #calculates proportion of each LC in the muni (ignoring NAs, help from https://stackoverflow.com/a/44290753)
  data %>%
    group_by(muniID) %>%
    dplyr::summarise(LC1 = round(sum(lc == 1, na.rm = T) / sum(!is.na(lc)), 3),
                     LC2 = round(sum(lc == 2, na.rm = T) / sum(!is.na(lc)), 3),
                     LC3 = round(sum(lc == 3, na.rm = T) / sum(!is.na(lc)), 3),
                     LC4 = round(sum(lc == 4, na.rm = T) / sum(!is.na(lc)), 3),
                     LC5 = round(sum(lc == 5, na.rm = T) / sum(!is.na(lc)), 3),
                     NonNAs = sum(!is.na(lc)),
                     NAs = sum(is.na(lc))
    ) -> LCs

  return(LCs)
}
######

years <- seq(2001,2018,1)
  
input_path <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/"

#load the rasters
munis.r <- raster(paste0(input_path,"CRAFTYInput/Data/sim10_BRmunis_latlon_5km.asc"))

lcname <- paste0(input_path,"CRAFTYInput/Data/ObservedLCmaps/LandCover2015_PastureB_Disagg.asc")
lc <- raster(lcname)
lc.t <- extractXYZ(lc, addCellID = F)


state_weights <- read_excel(paste0(input_path,"CRAFTYInput/Data/HumanDev/StateWeightedCaps.xlsx"), sheet = "Human", range="A1:U11", col_names=T)  

state_weights <- state_weights %>%
  mutate_if(is.numeric, round, digits=3)

munis.t <- extractXYZ(munis.r, addCellID = F)
munis.t <- as.data.frame(munis.t)
munis.t <- plyr::rename(munis.t, c("vals" = "muniID"))

#set NA in both rasters
lc[is.na(munis.r)] <- NA
munis.r[is.na(lc)] <- NA
  
lc_munis <- left_join(as.data.frame(munis.t), as.data.frame(lc.t), by = c("row" = "row", "col" = "col"))

#add state label
#add state ID
lc_munis <- lc_munis %>%
  mutate(state = (muniID %/% 100000)) %>%
  mutate(state = if_else(state == 17, "TO", 
      if_else(state == 29, "BA",
      if_else(state == 31, "MG",
      if_else(state == 35, "SP",
      if_else(state == 41, "PR",
      if_else(state == 42, "SC",
      if_else(state == 43, "RS", 
      if_else(state == 50, "MS",
      if_else(state == 51, "MT",
      if_else(state == 52, "GO", "NA"
      ))))))))))
    )



new_munis <- left_join(lc_munis, state_weights, by = c("state" = "state"))

#set emptyto a raster with same extent as inputs (to the same) with help from https://gis.stackexchange.com/questions/250149/assign-values-to-a-subset-of-cells-of-a-raster)
empty.r <- raster(munis.r)
empty.r[] <- NA_real_
cells <- cellFromRowCol(empty.r, new_munis$row, new_munis$col)


for(year in years){
  
  year_dat <- new_munis %>%
    select(paste0(year))

  final.r <- empty.r    
  final.r[cells] <- year_dat[[1]]
  
  #plot(final.r, main=paste0(year))
  
  writeRaster(final.r, paste0(input_path,"CRAFTYInput/Data/HumanDev/HumanCapital",year,".asc"), format = 'ascii', overwrite=T)
}




