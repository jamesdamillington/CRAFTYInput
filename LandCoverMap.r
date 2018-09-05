

rm(list=ls())

library(raster)
library(tidyverse)



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
#(this is where issued with muni 5006275 was discovered
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


j <- joined %>%
  dplyr::select(muniID, A_mapped_cells, OA_mapped_cells, A_plant_cells, OA_plant_cells) %>%
  mutate(A_diffc = A_mapped_cells - A_plant_cells) %>%
  mutate(OA_diffc = OA_mapped_cells - OA_plant_cells)


#case 1
#if OA_mapped > OA_planted AND A_mapped < A_planted
  #then take enough from difference so A_mapped == A_planted, any remainder is pasture
j <- j %>%
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
j <- j %>%
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
    
j <- j %>%
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
j <- j %>%
  mutate(OA_final = 
    if_else(OA_diffc == 0 & A_diffc < 0, 0, OA_final)) %>%
  mutate(A_final = 
    if_else(OA_diffc == 0 & A_diffc < 0, 0, A_final)) %>%
  mutate(P_final = 
    if_else(OA_diffc == 0 & A_diffc < 0, 0, P_final))

#case 4
j <- j %>%
  mutate(OA_final = 
    if_else(OA_diffc == 0 & A_diffc >= 0, 0, OA_final)) %>%
  mutate(A_final = 
    if_else(OA_diffc == 0 & A_diffc >= 0, 0, A_final)) %>%
  mutate(P_final = 
    if_else(OA_diffc == 0 & A_diffc >= 0, 0, P_final))

#case 5
j <- j %>%
  mutate(OA_final = 
    if_else(OA_diffc < 0 & A_diffc <= 0, 0, OA_final)) %>%
  mutate(A_final = 
    if_else(OA_diffc < 0 & A_diffc <= 0, 0, A_final)) %>%
  mutate(P_final = 
    if_else(OA_diffc < 0 & A_diffc <= 0, 0, P_final))

#check all cells have changed
#k <- j %>%
#  filter(OA_final == 99)

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