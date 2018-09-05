

rm(list=ls())

library(raster)
library(tidyverse)



#read Summary table for 2001 - this contains number of cells in each muni (and proportions in each LC)


#read planted area data

read_csv(PlantedArea_2000-2003.csv


#From planted area data for 2001 calculate muni proportions (of total area) of:
- Soybean + Maize  [A_plant]
- Cotton + Rice + Sugar_Cane + Bean + Sorghum + Wheat [OA_plant]

#Then for 2003 planted area data calculate relative proportion of:
- Soy (Soy > 0 and M_second_crop < 100)  
- Maize (Maize > 0 and M_second_crop < 100)
- Double crop (Soy > 0 and M_second_crop > 100)

#if soybean_2003 == 0; double-crop = 0
#if Maize_second_crop2003 < 100; double-crop = 0

From mapbiomas data calculate proportions (of municipality) of:
- agriculture
- OAgri

Calculate number of OAgri cells needed to match OA_plant:
- set number of Oagri cells to match planted proportion

#then for remaining Agri cells assign relative prportion of soy, maize, DC