## ---------------------------
##
## Script name: 03_MaxEnt_Prep
##
## Purpose of script: Extract environmental Covariates @ occurence points and prepare data for MaxEnt Model
##
## Author: Trent VanHawkins
##
## Date Created: 2024-03-04
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


# Read in Data ------------------------------------------------------------
spp_occ <- read_csv(here("DataProcessed.nosync/SpeciesOccurrence/spp_occ_master.csv"))
covars <- rast(here("DataProcessed.nosync/Covariates/env_covars.tif"))
pnw <- st_read(here("DataProcessed.nosync/Covariates/pnw_buff.gpkg"))

# Convert spp_occ to sf object --------------------------------------------
## Converting spp_occ data frame to spatial object
spp_spat <- st_as_sf(spp_occ, coords = c("Longitude", "Latitude"), crs = "WGS84")

## Crop out Northern CA Points
spp_spat <- st_crop(spp_spat, pnw)


# Extract environemental covariates ---------------------------------------
spp_covs <- terra::extract(covars, spp_spat, bind = T)


# Do some data exploration ------------------------------------------------

ggplot()+
  geom_sf(data = spp_covs, aes(color = `Water (%)`))

          