#script to redistribute OtherAgriculture and Agriculture cells based on planted data 

rm(list=ls())

library(raster)
library(tidyverse)

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



#converts data in CRAFTY output file for a single variable and creates a raster
outputRaster <- function(data, variable){
  
  out <- data %>%
    dplyr::select(X, Y, !!variable)
  
  ras <- rasterFromXYZ(out)
  
  return(ras)
  
}


#function() for each municipality:
#- compare A obs and desired; change if necessary (increase from OA, decrease to OA)
#- compare OA obs and desired; change if necessary (increase from Pas, decrease to Pas)
#convs is the conversions calculated above, lcs is the lc_munis df
#both should already have been subst to a single muni
convertLCs <- function(convs, lcs) {
  
  #subset data
  NA_lcs <- filter(lcs, lc == 1 | lc == 4)  #not Agri or Pas
  P_lcs  <- filter(lcs, lc == 5) 
  A_lcs <- filter(lcs, lc == 3)
  OA_lcs <- filter(lcs, lc == 2)

  A_obs <- length(A_lcs$lc)        #for reporting
  A_diffc = convs$A_final - A_obs  #counter of how many conversions needed

  # print(paste0("A_obs: ",A_obs))
  # print(paste0("A_target: ",convs$A_final))
  # print(paste0("A_diffc: ",A_diffc))

  ctr <- 1 #counter to ensure we don't try to access beyond length of tables below
  
  # print(OA_lcs)
  
  #increase A from OA (2 -> 3)
  if(is.integer(A_diffc))  #in case where there are no A pixels, A_diffc is not integer 
  {
    while(A_diffc > 0) {
      if(!any(2 %in% OA_lcs$lc)) break   #if there are no values to convert, break
      if(ctr > length(OA_lcs$lc)) break  #if we are trying to convert more values than available, break
      
      OA_lcs[ctr,4] <- 3                 #do the conversion
     
      ctr <- ctr + 1                     #update counter
      A_diffc <- A_diffc - 1             #update counter
    }  
    
    # print(OA_lcs)

    # print(A_lcs)

    ctr <- 1
    #decrease A to OA (3 -> 2)
    while(A_diffc < 0) {
      if(!any(3 %in% A_lcs$lc)) break
      if(ctr > length(A_lcs$lc)) break
      
      A_lcs[ctr,4] <- 2
      
      ctr <- ctr + 1
      A_diffc <- A_diffc + 1
    }
  }
  
  # print(A_lcs)
  
  #update data as we may have made some conversions that have changed OA_lcs (makes OA conversions below more accurate)
  olcs <- bind_rows(NA_lcs, A_lcs, OA_lcs, P_lcs)
  NA_lcs <- filter(olcs, lc == 1 | lc == 4)  #not Agri or Pas
  P_lcs  <- filter(olcs, lc == 5) 
  A_lcs <- filter(olcs, lc == 3)
  OA_lcs <- filter(olcs, lc == 2)

  OA_obs <- length(OA_lcs$lc)
  OA_diffc = convs$OA_final - OA_obs

  # print(paste0("OA_obs: ",OA_obs))
  # print(paste0("OA_target: ",convs$OA_final))
  # print(paste0("OA_diffc: ",OA_diffc))
  # print(OA_lcs)
  
  ctr <- 1
  
  #increase OA from Pas (5 -> 2)
  if(is.integer(OA_diffc))  #in case where there are no OA pixels, OA_diffc is not integer 
  {
    while(OA_diffc > 0) {
      if(!any(5 %in% P_lcs$lc)) break
      if(ctr > length(P_lcs$lc)) break
      
      P_lcs[ctr,4] <- 2
  
      ctr <- ctr + 1
      OA_diffc <- OA_diffc - 1
    }
  
    ctr <- 1
    #decrease OA to Pas (2 -> 5)
    while(OA_diffc < 0) {
      if(!any(2 %in% OA_lcs$lc)) break
      if(ctr > length(OA_lcs$lc)) break
      
      OA_lcs[ctr,4] <- 5
      
      ctr <- ctr + 1
      OA_diffc <- OA_diffc + 1
    }
  }
  
  #print(OA_lcs)
  
  olcs <- bind_rows(NA_lcs, A_lcs, OA_lcs, P_lcs)
  
  #used during testing
  # if(length(lcs$lc) != length(olcs$lc))
  # {  
  #   print(paste0("lcs: ", length(lcs$lc)))
  #   print(lcs)
  #   
  #   print(paste0("olcs: ", length(olcs$lc)))
  #   print(olcs)
  #   
  # }

    
  # print(paste0("A_obs: ",length(filter(lcs, lc == 3)[,4])))
  # print(paste0("A_target: ",convs$A_final))
  # print(paste0("OA_obs: ",length(filter(lcs, lc == 2)[,4])))
  # print(paste0("OA_target: ",convs$OA_final))
   
  return(olcs)
}



#read Summary table for 2001 - this contains number of cells in each muni (and proportions in each LC)
mapped <- read_csv("Data/ObservedLCmaps/LCs2001_PastureB.csv")

# From mapbiomas data calculate number of cells for:
# - agriculture
# - OAgri
mapped <- mapped %>%
  mutate(A_mapped_cells = round(LC3 * NonNAs,0)) %>%
  mutate(OA_mapped_cells = round(LC2 * NonNAs,0))


#muni 5006275 was only created in 2013, partitioned from 5000203
#so add values from 5006275 to 5000203
old <- mapped$A_mapped_cells[mapped$muniID == 5000203]
new <- mapped$A_mapped_cells[mapped$muniID == 5006275] + old
mapped$A_mapped_cells[mapped$muniID == 5000203] <- new

old <- mapped$OA_mapped_cells[mapped$muniID == 5000203]
new <- mapped$OA_mapped_cells[mapped$muniID == 5006275] + old
mapped$OA_mapped_cells[mapped$muniID == 5000203] <- new 

#mapped %>%
#  filter(A_mapped_cells > 0) %>%
#  ggplot(aes(x = A_mapped_cells)) +
#  geom_histogram(binwidth=5)
  

#read planted area data
planted <- read_csv("Data/ObservedLCmaps/PlantedArea_2000-2003.csv")

# #From planted area data for 2001 calculate number of cells for:
# - Soybean + Maize  [A_plant]
# - Cotton + Rice + Sugar_Cane + Bean + Sorghum + Wheat [OA_plant]
planted <- planted %>%
  mutate(A_plant_ha = Maize + Soybean) %>%
  mutate(OA_plant_ha = Cotton + Rice + Sugar_Cane + Bean + Sorghum + Wheat) %>%
  mutate(A_plant_cells = round(A_plant_ha / 2500, 0)) %>%
  mutate(OA_plant_cells = round(OA_plant_ha / 2500, 0))  #one cell = 2500ha

#join the data
joined <- left_join(mapped, planted, by = c("muniID" = "IBGE_CODE"))

#previously used to check the join 
#(this is where issue with muni 5006275 was discovered
#munis 4300001 and 4300002 are also missing, but these are large lakes with minimal agriculture
#missing <- joined %>% 
#  filter(is.na(A_plant_cells))



# Calculate number of OAgri, Agri and Pasture cells needed to match OA_plant and A_plant:
#Overall A_final + OA_final + P_final must equal A_mapped + OA_mapped
#So:
#case 1
#if OA_mapped > OA_planted AND A_mapped < A_planted
  #then take enough from difference so A_mapped == A_planted, any remainder is pasture 
#case 2
#if OA_mapped > OA_planted AND A_mapped >= A_planted
  #then OA is planted value and remainder is pasture, A_mapped does not change
#case 3
#if OA_mapped == OA_planted AND A_mapped < A_planted
  #then nothing changes
#case 4
#if OA_mapped == OA_planted AND A_mapped >= A_planted
  #then nothing changes
#case 5
#if OA_mapped < OA_planted AND A_mapped <= A_planted
  #then nothing changes
#case 6
#if OA_mapped < OA_planted AND A_mapped > A_planted
  #then add difference from A to OA (so that OA_M == OA_p, or until A_m == A_p) 


#calculate differences (used in cases below)
joined <- joined %>%
  dplyr::select(muniID, A_mapped_cells, OA_mapped_cells, A_plant_cells, OA_plant_cells) %>%
  mutate(A_diffc = A_mapped_cells - A_plant_cells) %>%
  mutate(OA_diffc = OA_mapped_cells - OA_plant_cells)

#case 1
#if OA_mapped > OA_planted AND A_mapped < A_planted
  #then take enough from difference so A_mapped == A_planted, any remainder is pasture
joined <- joined %>%
  mutate(OA_final = 
    if_else(OA_diffc > 0 & A_diffc < 0, OA_plant_cells,99))  %>%  
  mutate(A_final = 
    if_else(OA_diffc > 0 & A_diffc < 0, 
      if_else(OA_diffc >= abs(A_diffc), A_plant_cells, A_mapped_cells + OA_diffc),
      99)) %>%
  mutate(P_final = 
    if_else(OA_diffc > 0 & A_diffc < 0,
      if_else(OA_diffc >= abs(A_diffc), OA_mapped_cells - OA_plant_cells - abs(A_diffc), 0),
      99))

#case 2
#if OA_mapped > OA_planted AND A_mapped >= A_planted
  #then OA is planted value and remainder is pasture, A_mapped does not change
joined <- joined %>%
  mutate(OA_final = 
    if_else(OA_diffc > 0 & A_diffc >= 0, OA_plant_cells, OA_final)) %>%
  mutate(A_final = 
      if_else(OA_diffc > 0 & A_diffc >= 0, A_plant_cells, A_final)) %>%
  mutate(P_final = 
    if_else(OA_diffc > 0 & A_diffc >= 0, OA_diffc, P_final))

#case 6
#if OA_mapped < OA_planted AND A_mapped > A_planted
  #then add difference from A to OA:
    #if A_diffc <= abs(OA_diffc) OA_final is max possible, otherwise OA_plant (A_final is always A_mapped - A_diffc)
    #nothing goes to P_final
joined <- joined %>%
  mutate(OA_final = 
    if_else(OA_diffc < 0 & A_diffc > 0,
      if_else(A_diffc <= abs(OA_diffc), OA_mapped_cells + A_diffc, OA_mapped_cells + abs(OA_diffc)),
      OA_final)) %>%
  mutate(A_final = 
    if_else(OA_diffc < 0 & A_diffc > 0, 
      if_else(A_diffc <= abs(OA_diffc), A_mapped_cells - A_diffc, A_mapped_cells - abs(OA_diffc)),
      A_final)) %>%
  mutate(P_final = 
    if_else(OA_diffc < 0 & A_diffc > 0, 0, P_final))


#case 3
joined <- joined %>%
  mutate(OA_final = 
    if_else(OA_diffc == 0 & A_diffc < 0, 0, OA_final)) %>%
  mutate(A_final = 
    if_else(OA_diffc == 0 & A_diffc < 0, 0, A_final)) %>%
  mutate(P_final = 
    if_else(OA_diffc == 0 & A_diffc < 0, 0, P_final))

#case 4
joined <- joined %>%
  mutate(OA_final = 
    if_else(OA_diffc == 0 & A_diffc >= 0, 0, OA_final)) %>%
  mutate(A_final = 
    if_else(OA_diffc == 0 & A_diffc >= 0, 0, A_final)) %>%
  mutate(P_final = 
    if_else(OA_diffc == 0 & A_diffc >= 0, 0, P_final))

#case 5
joined <- joined %>%
  mutate(OA_final = 
    if_else(OA_diffc < 0 & A_diffc <= 0, 0, OA_final)) %>%
  mutate(A_final = 
    if_else(OA_diffc < 0 & A_diffc <= 0, 0, A_final)) %>%
  mutate(P_final = 
    if_else(OA_diffc < 0 & A_diffc <= 0, 0, P_final))

#check all cells have changed
#k <- j %>%
#  filter(OA_final == 99)

#now update map
#read muniID map -> get x,y,z
input_path <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/"

#load the rasters
munis.r <- raster(paste0(input_path,"CRAFTYInput/Data/sim10_BRmunis_latlon_5km_2018-04-27.asc"))
lc.r <- raster("Data/ObservedLCmaps/brazillc_2001_PastureB.asc")

munis.t <- extractXYZ(munis.r, addCellID = F)
lc.t <- extractXYZ(lc.r, addCellID = F)

munis.t <- as.data.frame(munis.t)
munis.t <- plyr::rename(munis.t, c("vals" = "muniID"))
  
lc.t <- as.data.frame(lc.t)
lc.t <- plyr::rename(lc.t, c("vals" = "lc"))
 

#join observed land cover map (so have x,y,muniID,original LC
lc_munis <- left_join(as.data.frame(munis.t), as.data.frame(lc.t), by = c("row" = "row", "col" = "col"))

#note: missing cells after join 
#lcNA <- lc_munis %>% filter(is.na(lc)) 

#for testing
#this.muniID <- 4202073
#lcs <- filter(lc_munis, muniID == this.muniID)
#convs <- filter(j, muniID == this.muniID)
#convertLCs(convs, lcs)

final <- data.frame() 

#for testing
#dummy <- c(3527603,3527603,3527504,3527504,3528205)
  
#loop through all munis to update https://stackoverflow.com/a/13916342/10219907
for(i in 1:length(unique(lc_munis$muniID))) {

#for(i in 1:length(unique(dummy))) {  
  
  this.muniID <- unique(lc_munis$muniID)[i]
    
  #this.muniID <- unique(dummy)[i]
  print(this.muniID)
  
  lcs <- filter(lc_munis, muniID == this.muniID)
  convs <- filter(joined, muniID == this.muniID)
   
  this.conv <- convertLCs(convs, lcs)
  
  if(i == 1) final <- this.conv
  else final <- bind_rows(final, this.conv)
  
}

#set final to a raster with same extent as inputs (to the same)with help from https://gis.stackexchange.com/questions/250149/assign-values-to-a-subset-of-cells-of-a-raster)
final.r <- raster(munis.r)
final.r[] <- NA_real_
cells <- cellFromRowCol(final.r, final$row, final$col)
final.r[cells] <- final$lc

writeRaster(final.r, "Data/ObservedLCmaps/NewAgri_brazillc_2001_PastureB.asc", format = 'ascii', overwrite=T)


# 
# #Then for 2003 planted area data calculate relative proportion of:
# - Soy (Soy > 0 and M_second_crop < 100)  
# - Maize (Maize > 0 and M_second_crop < 100)
# - Double crop (Soy > 0 and M_second_crop > 100)
# 
# #if soybean_2003 == 0; double-crop = 0
# #if Maize_second_crop2003 < 100; double-crop = 0
# 

# 
#then for remaining Agri cells assign relative prportion of soy, maize, DC