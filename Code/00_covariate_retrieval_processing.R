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
require(magrittr)
require(here) #Package for using relative pathnames
require(geodata) #Package for downloading WorldClim data
require(maps)
require(sf)
require(raster)
require(ggmap)
require(FedData)

# WorldClim 2.0 -----------------------------------------------------------
#Us State Polygons from "maps" package
us_states <- st_as_sf(map("state", plot = F, fill = T))

#Subset to the given region and project to match variable of interest
targ_states <- c("Oregon", "Washington", "Idaho")

pnw <- us_states %>% 
  filter(ID %in% tolower(targ_states)) %>% 
  st_transform(crs = st_crs("WGS84"))

## Download Monthy Average Temp and Precip from WorldClim
tavg <- worldclim_country(country = "United States", res = 0.5, var = "tavg", path = here("DataRaw.nosync/Covariates/WorldClim2"))
prec <- worldclim_country(country = "United States", res = 0.5, var = "prec", path = here("DataRaw.nosync/Covariates/WorldClim2"))
elev <- elevatr::get_elev_raster(pnw, z = 8, clip = "locations")
nlcd <- get_nlcd(pnw, label = "PNW", year = 2021, extraction.dir = here("DataRaw.nosync/Covariates/NLCD"))

# Convert Monthly Avg to Annual Avg ---------------------------------------
tavg <- mean(tavg)
prec <- mean(prec)


# Get Forest Cover, Water, and Wetland (See https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description) --------------------------------------------------------
forest <- nlcd > 40 & nlcd < 50
water <-  nlcd < 20 
wetland <- nlcd >= 90

test <- terra::project(forest, "epsg:4326")
# Function for projecting, cropping, and resampling -----------------------

crop_mask <- function(var, states = c("Oregon", "Washington", "Idaho"), target_res){
  if (class(var) != "SpatRaster"){
    var <- terra::rast(var)
  }
  #project the raster into WGS84
  var <- terra::project(var, "epsg:4326")
  #resample if not in the target resolution
  if (res(var) != target_res){
    resa
  }
  
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


