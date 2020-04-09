# CRAFTYInput
[![DOI](https://zenodo.org/badge/139715339.svg)](https://zenodo.org/badge/latestdoi/139715339)
Scripts to create input files for the [CRAFTY-Brazil](https://github.com/jamesdamillington/CRAFTY_Brazil) ABM simulation model. See also [CRAFTY-Brazil Input Maps](https://github.com/jamesdamillington/BrazilInputMaps) for scripts that create the base land cover map. 

There are two primary types of input file:
- [The region.csv file](#the-region.csv-file) used to initialise a model run
- [Capital update files](#capital-update-files) change Capital values during a model run 

## The region.csv file
Create the region.csv file using [create_CRAFTY_regionCSV.r](create_CRAFTY_regionCSV.r) (possibly created using [input map scripts](#input-map-scripts)) and other information (e.g. a base land cover map). 


### Input map scripts

To create data to simulate the observed time period 2001-2018:

- [moistureMap.r](moistureMap.r): creates Captial maps for either Moisture-Main or Moisture-Second (first and second seasons respectively). Requires output from [slopeMap.r](slopeMap.r) and [soilMap.r](soilMap.r) (see below) and [cru_ts4 climate](https://crudata.uea.ac.uk/cru/data/hrg/) data (to calculate water balance)   
- [PortAccessMap.r](PortAccessMap.r): resamples and rescales Transport Capital maps
- [LandProtectionMap.r](LandProtectionMap.r): rasterizes shapefiles to create maps for Land Protection Capitals 
- [LandValueMap.r](LandValueMap.r): rasterizes land price vector map and classifies for the Land Value Capital
- [accessMap.r](accessMap.r): create Access Capital maps from land cover maps [see CRAFTY-Brazil Input Maps](https://github.com/jamesdamillington/BrazilInputMaps))

Helper scripts for the scripts above:
- [slopeMap.r](slopeMap.r): converts original high-res slope map to resolution for model and classifies this as a Capital. Used in [moistureMap.r](moistureMap.r)
- [soilMap.r](soilMap.r): converts original low-res soil map to resolution for model and classifies as a Capital. Used in [moistureMap.r](moistureMap.r)

To create data to simulate a future time period:

- [moistureMap_future.r](moistureMap_future.r): replaces output from [moistureMap.r](moistureMap.r) to forecast future water balance. Requires scripts are for [moistureMap.r](moistureMap.r) in addition to data from a GCM (in this version output from [HadGEM2](https://portal.enes.org/models/earthsystem-models/metoffice-hadley-centre/hadgem2-es) for different [IPCC RCPs](https://en.wikipedia.org/wiki/Representative_Concentration_Pathway), available from [CEDA](https://esgf-index1.ceda.ac.uk/search/cordex-ceda/)) 


## Capital update files

- [createUpdateFiles.r](createUpdateFiles.r): takes input map files - possibly created using some of the [input map scripts](#input-map-scripts) - and converts to a table that can be used to update Capitals during a model run
- [createSingleLCMap.r](createSingleLCMap.r): create maps of a single land cover from observed land cover maps (useful for updating 'Other Agri' and 'Other' Capitals through simulation
