## ---------------------------
##
## Script name: 05_Application
##
## Purpose of script: Worked example for prediction raster
##
## Author: Trent VanHawkins
##
## Date Created: 2024-04-04
##
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## ---------------------------

## view outputs in non-scientific notation

options(scipen = 6, digits = 4) 

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(here)
require(terra)
require(tidyterra)
require(sf)

block.res <- rast(here("DataProcessed.nosync/ModResults/all_blockres.tiff"))
odfw <- sf::read_sf(here("DataRaw.nosync/Application/ODFW_1182_5_odfw_bnds/odfw_bnds.shp"))

odfw %>% 
  filter(H2O_DIST_N == "Deschutes") %>% 
  plot()
