# CRAFTYInput
Scripts to create input files for CRAFTY (Brazil)

There are two primary types of input file:
- [The region.csv file](#the-region.csv-file) used to initialise a model run
- [Capital update files](#capital-update-files) change Capital values during a model run 

## The region.csv file
Two scripts can be used to create a region.csv file:
 
1. [create_CRAFTY_regionCSV.r](create_CRAFTY_regionCSV.r) is used to create input data for a real world situation using input maps - possibly created using [input map scripts](#input-map-scripts) - and other information. 
2. [create_null_CRAFTY_regionCSV.r](create_null_CRAFTY_regionCSV.r) generates a null (or random) set of input data 

### Input map scripts

- [agricultureMap.r](agricultureMap.r): creates an an agriculture Captial map. Requires output from [slopeMap.r](slopeMap.r) and [soilMap.r](soilMap.r) (see below) and [cru_ts4 climate](https://crudata.uea.ac.uk/cru/data/hrg/) data (to calculate water balance).   
- [LandCoverMap.r](LandCoverMap.r): reclassifies original MapBiomas land cover map [(currently version 2.3)](http://mapbiomas.org/pages/database/mapbiomas_collection) to the classes used in the model
- [LandPriceMap.r](LandPriceMap.r): rasterizes land price vector map and classifies as a Capital.
- [slopeMap.r](slopeMap.r): converts original high-res slope map to resolution for model and classifies this as a Capital. Used in [agricultureMap.r](agricultureMap.r)
- [soilMap.r](soilMap.r): converts original low-res soil map to resolution for model and classifies as a Capital. Used in [agricultureMap.r](agricultureMap.r)

## Capital update files

- [createUpdateFiles.r](createUpdateFiles.r): takes input map files - possibly created using some of the [input map scripts](#input-map-scripts) - and converts to a table that can be used to update Capitals during a model run
- [createSingleLCMap.r](createSingleLCMap.r): create maps of a single land cover from observed land cover maps (useful for updating 'Other Agri' and 'Other' Capitals through simulation
