#Creating CRAFTY input data - region.csv
#2017-08-14 

rm(list=ls())

library(raster)
library(tidyverse)


######
#FUNCTIONS
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


#####
belmontDataPath <- "C:/Users/k1076631/Google Drive/Research/Projects/Belmont/Data/"
setwd(paste(belmontDataPath, "Brazil/AdminBoundaries/IBGE", sep=""))

#read base raster (e.g. from create_base_raster.r)
munis.r <- raster("sim_BRmunis_utm_5km.asc")
plot(munis.r)

#unique values from muni id raster
u.mids <- unique(munis.r)
length(u.mids)



#create reclassify matrix: muni ids and a random value (uniform for entire muni)
set.seed(1)

#categorical
br <- cbind(u.mids, sample(c(1:5), length(u.mids), replace = T))
fr <- cbind(u.mids, sample(c(1:5), length(u.mids), replace = T))
agentID <- cbind(u.mids, sample(c(1:5), length(u.mids), replace = T))

##choose either sample or read from file
#continuous: sampled 
# agri <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# nat <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# hum <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# dev <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# infra <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# econ <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))
# access <- cbind(u.mids, round(runif(length(u.mids), 0, 1),3))

#continuous: from file
setwd(paste(belmontDataPath, "Brazil/AdminBoundaries/CRAFTYtesting", sep=""))
dat <- read.csv("createRegionCSV_dummy.csv")

#join data to u.mids in the raster (some may be missing)
df.u.mids <- as.data.frame(u.mids)
dat <- inner_join(dat, df.u.mids, by = c('CD_GCMUN' = 'u.mids'))

agri <- dat[,c('CD_GCMUN','Agriculture')]
nat <- dat[,c('CD_GCMUN','Nature')]
hum <- dat[,c('CD_GCMUN','Human')]
dev <- dat[,c('CD_GCMUN','Development')]
infra <- dat[,c('CD_GCMUN','Infrastructure')]
econ <- dat[,c('CD_GCMUN','Economic')]
access <- dat[,c('CD_GCMUN','Accessibility')]


#use reclassify matrix 
br.r <- reclassify(munis.r, br)
fr.r <- reclassify(munis.r, fr)
agentID.r <- reclassify(munis.r, agentID)
agri.r <- reclassify(munis.r, agri)
nat.r <- reclassify(munis.r, nat)
hum.r <- reclassify(munis.r, hum)
dev.r <- reclassify(munis.r, dev)
infra.r <- reclassify(munis.r, infra)
econ.r <- reclassify(munis.r, econ)
access.r <- reclassify(munis.r, access)

munis.s <- stack(list(munis.r,
                      br.r,
                      fr.r,
                      agentID.r, 
                      agri.r, 
                      nat.r, 
                      hum.r, 
                      dev.r, 
                      infra.r, 
                      econ.r, 
                      access.r))

names(munis.s) <- list("muniID",
                       "BR",
                       "FR",
                       "agentID", 
                       "agri",
                       "nat",
                       "hum",
                       "dev",
                       "infra",
                       "econ",
                       "access")

munis.out <- extractXYZ(munis.s)
head(munis.out)

#need to check row,col consistency with CRAFTY X,Y
write.csv(munis.out,"region.csv", row.names = F)



###################################################

#reclassify from matrix of muni ids and mean, SD (normal distribution)
# - create two new rasters, one for mean, one for sd, then use these with raster arithmetic 

u.mids <- unique(munis.r)

#categorical variables are not randomised
set.seed(1)
br <- cbind(u.mids, sample(c(1:5), length(u.mids), replace = T))
fr <- cbind(u.mids, sample(c(1:5), length(u.mids), replace = T))
agentID <- cbind(u.mids, sample(c(1:5), length(u.mids), replace = T))

#non-sampled
br.r <- reclassify(munis.r, br)
fr.r <- reclassify(munis.r, fr)
agentID.r <- reclassify(munis.r, agentID)

#following is random, alternatively #if want to pass value will need an input matrix of format:
#muni, br, fr, agentID, agri mean, agri sd, ...  (row per muni) - what to do about categorical?

#create lookup table of mean muni values
agri_mn <- cbind(u.mids, runif(length(u.mids), 0.3, 0.7))
nat_mn <- cbind(u.mids, runif(length(u.mids), 0.3, 0.7))
hum_mn <- cbind(u.mids, runif(length(u.mids), 0.3, 0.7))
dev_mn <- cbind(u.mids, runif(length(u.mids), 0.3, 0.7))
infra_mn <- cbind(u.mids, runif(length(u.mids), 0.3, 0.7))
econ_mn <- cbind(u.mids, runif(length(u.mids), 0.3, 0.7))
access_mn <- cbind(u.mids, runif(length(u.mids), 0.3, 0.7))

#create lookup table of sd muni values
agri_sd <- cbind(u.mids, runif(length(u.mids), 0, 0.2))
nat_sd <- cbind(u.mids, runif(length(u.mids), 0, 0.2))
hum_sd <- cbind(u.mids, runif(length(u.mids), 0, 0.2))
dev_sd <- cbind(u.mids, runif(length(u.mids), 0, 0.2))
infra_sd <- cbind(u.mids, runif(length(u.mids), 0, 0.2))
econ_sd <- cbind(u.mids, runif(length(u.mids), 0, 0.2))
access_sd <- cbind(u.mids, runif(length(u.mids), 0, 0.2))

#create raster of mean value (identical by muni)
agri_mn.r <- reclassify(munis.r, agri_mn)
nat_mn.r <- reclassify(munis.r, nat_mn)
hum_mn.r <- reclassify(munis.r, hum_mn)
dev_mn.r <- reclassify(munis.r, dev_mn)
infra_mn.r <- reclassify(munis.r, infra_mn)
econ_mn.r <- reclassify(munis.r, econ_mn)
access_mn.r <- reclassify(munis.r, access_mn)

#create raster of sd value (identical by muni)
agri_sd.r <- reclassify(munis.r, agri_sd)
nat_sd.r <- reclassify(munis.r, nat_sd)
hum_sd.r <- reclassify(munis.r, hum_sd)
dev_sd.r <- reclassify(munis.r, dev_sd)
infra_sd.r <- reclassify(munis.r, infra_sd)
econ_sd.r <- reclassify(munis.r, econ_sd)
access_sd.r <- reclassify(munis.r, access_sd)


#extimate cell values using mean and sd from rasters
#from https://gis.stackexchange.com/a/209245
rastnorm <- function(x){ { return(rnorm(1, mean = x[[1]], sd = x[[2]])) }}

set.seed(1)
agri_rnorm.r <- calc(stack(agri_mn.r, agri_sd.r), fun = rastnorm)
nat_rnorm.r <- calc(stack(nat_mn.r, nat_sd.r), fun = rastnorm)
hum_rnorm.r <- calc(stack(hum_mn.r, hum_sd.r), fun = rastnorm)
dev_rnorm.r <- calc(stack(dev_mn.r, dev_sd.r), fun = rastnorm)
infra_rnorm.r <- calc(stack(infra_mn.r, infra_sd.r), fun = rastnorm)
econ_rnorm.r <- calc(stack(econ_mn.r, econ_sd.r), fun = rastnorm)
access_rnorm.r <- calc(stack(access_mn.r, access_sd.r), fun = rastnorm)


rnorm.s <- stack(list(agri_rnorm.r, 
                      nat_rnorm.r, 
                      hum_rnorm.r, 
                      dev_rnorm.r, 
                      infra_rnorm.r, 
                      econ_rnorm.r, 
                      access_rnorm.r))

#reclassify rasters if <0 or >1
#from https://gis.stackexchange.com/a/167514
rc_norm <- function(x1) {  ifelse( x1 > 1, 1, ifelse( x1 < 0, 0, x1 )) }
rnorm.s <- calc(rnorm.s, fun=rc_norm)

#function to round raster layer values to 3 dp
r_round3 <- function(x1) { round(x1,3) }
rnorm.s <- calc(rnorm.s, fun=r_round3)

#unstack so we can re-stack with the other non-sampled layers
rnorm.l <- unstack(rnorm.s)

#new stack using purrr:: prepend 
munis_rnorm.l <- rnorm.l %>% prepend(list(munis.r,
                                     br.r,
                                     fr.r,
                                     agentID.r)
                                , before = 1)

#restack
munis_rnorm.s <- stack(munis_rnorm.l)

names(munis_rnorm.s) <- list("muniID",
                             "BR",
                             "FR",
                             "agentID", 
                             "agri",
                             "nat",
                             "hum",
                             "dev",
                             "infra",
                             "econ",
                             "access")


munis_rnorm.out <- extractXYZ(munis_rnorm.s)
head(munis_rnorm.out)

write.csv(munis_rnorm.out,"out5.csv", row.names = F)



