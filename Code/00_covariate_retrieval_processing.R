## ---------------------------
##
## Script name: 00_covariate_retrieval_processing.R
##
## Purpose of script: Pull and preprocess all covariate data needed for MaxEnt Modelling Pipeline
##
## Author: Trent VanHawkins
##
## Date Created: 2024-02-07
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
require(here) #Package for using relative pathnames
require(geodata) #Package for downloading WorldClim data
require(maps)
require(sf)
require(raster)
require(ggmap)
# WorldClim 2.0 -----------------------------------------------------------
## Download Monthy Average Temp and Precip from WorldClim
tavg <- worldclim_country(country = "United States", res = 2.5, var = "tavg", path = here("DataRaw.nosync/Covariates/WorldClim2"))
prec <- worldclim_country(country = "United States", res = 2.5, var = "prec", path = here("DataRaw.nosync/Covariates/WorldClim2"))


# Convert Monthly Avg to Annual Avg ---------------------------------------
tavg <- mean(tavg)
prec <- mean(prec)

crop_mask <- function(var, states = c("Oregon", "Washington", "Idaho")){
  #Us State Polygons from "maps" package
  us_states <- st_as_sf(map("state", plot = F, fill = T))
  
  #Subset to the given region and project to match variable of interest
  reg <- us_states %>% 
    filter(ID %in% tolower(states)) %>% 
    st_transform(crs = st_crs(var))
  
  #Crop and Mask variable to region
  crp <- crop(var, extent(reg))
  msk <- mask(crp, reg)
  
  #Plot result to make sure it is correct
  plot(msk)
  #Return the result
  return(msk)
}

tavg_pnw <- crop_mask(tavg)
prec_pnw <- crop_mask(prec)

ggmap()

