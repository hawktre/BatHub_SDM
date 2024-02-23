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
require(viridis)

# WorldClim 2.0 -----------------------------------------------------------
#Us State Polygons from "maps" package
us_states <- st_as_sf(map("state", plot = F, fill = T)) %>% 
  st_transform(crs = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83')

#Subset to the given region and project to match variable of interest
targ_states <- c("Oregon", "Washington", "Idaho")

pnw <- us_states %>% 
  filter(ID %in% tolower(targ_states))

pnw <- pnw %>% 
  st_make_valid() %>% 
  st_buffer(10000) %>% 
  st_union() %>% 
  st_transform(crs = "WGS84") %>% 
  st_sf() %>% 
  st_cast()

plot(pnw)
## Download Monthy Average Temp and Precip from WorldClim
tavg <- worldclim_country(country = "United States", res = 0.5, var = "tavg", path = here("DataRaw.nosync/Covariates/WorldClim2"))
prec <- worldclim_country(country = "United States", res = 0.5, var = "prec", path = here("DataRaw.nosync/Covariates/WorldClim2"))
elev <- elevatr::get_elev_raster(pnw, z = 8, clip = "locations")
nlcd <- get_nlcd(pnw, label = "PNW", year = 2021, extraction.dir = here("DataRaw.nosync/Covariates/NLCD"))
ergo <- terra::rast(here("DataRaw.nosync/Covariates/Ecologically_re/lf3_30T_GNLCC/lf3_30T_GNLCC.tif"))
landfire_or <- terra::rast(here("DataRaw.nosync/Covariates/LandFire/LF2022_OR/LC22_EVT_230.tif"))
landfire_wa <- rast(here("DataRaw.nosync/Covariates/LandFire/LF2022_WA/LC22_EVT_230.tif"))
landfire_id <- rast(here("DataRaw.nosync/Covariates/LandFire/LF2022_ID/LC22_EVT_230.tif"))


# Merge LandFire States into one raster -----------------------------------
landfire <- terra::merge(landfire_or, landfire_wa, landfire_id, first = T)

## subset all cliffs and canyons categories
lf_vals <- as.data.frame(unique(landfire)) %>% 
  filter(str_detect(EVT_NAME, "Cliff") | str_detect(EVT_NAME, "Canyon"))

## Keep cliffs and canyons only
cliff_canyon <- landfire %in% lf_vals$EVT_NAME
# Take Mean of WorldClim Layers -------------------------------------------

temp_precip <- c(tavg, prec)

temp_precip_avg <- tapp(temp_precip, index = c(rep(1, length(names(tavg))), 
                                               rep(2, length(names(prec)))),
                        terra::mean,
                        cores=parallel::detectCores() )

# Function for projecting, cropping, and resampling -----------------------
crop_mask <- function(var, states = c("Oregon", "Washington", "Idaho"), target_res){
  if (class(var) != "SpatRaster"){
    var <- terra::rast(var)
  }
  
  #Change Coordinate System to WGS84
  var <- terra::project(var, "epsg:4326", threads = T)
  
  #Us State Polygons from "maps" package
  us_states <- st_as_sf(map("state", plot = F, fill = T))
  
  #Subset to the given region and project to match variable of interest
  reg <- us_states %>% 
    filter(ID %in% tolower(states)) %>% 
    st_transform(crs = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 
                 +y_0=0 +ellps=GRS80 +datum=NAD83') %>% 
    st_make_valid() %>% 
    st_buffer(10000) %>% 
    st_union() %>% 
    st_transform(crs = "WGS84") %>% 
    st_sf() %>% 
    st_cast()
  
  #Crop and Mask variable to region
  crp <- crop(var, extent(reg))
  msk <- mask(crp, reg)
  
  #Plot result to make sure it is correct
  plot(msk)
  #Return the result
  return(msk)
}

#Get all Layers in same extent and projection
covars <- list(temp_precip_avg, nlcd, elev, ergo, cliff_canyon)

covars_crop <- lapply(covars, crop_mask)

# Extract Needed Values from NLCD -----------------------------------------

# Get Forest Cover, Water, and Wetland (See https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description) --------------------------------------------------------
forest <- covars_crop[[2]] > 40 & covars_crop[[2]] < 50
water <-  covars_crop[[2]] < 20 
wetland <- covars_crop[[2]] >= 90

covars <- list("tmp_precip" = covars_crop[[1]], 
               "elev" = covars_crop[[3]], 
               "ergo" = covars_crop[[4]],
               "landcov" = c(forest, water, wetland),
               "cliff_canyon" = covars_crop[[5]])

for (i in 2:length(covars)) {
  print(names(covars[[i]]))
  #Resample all rasters to have the same resolution and extent as tmp_precip
  covars[[i]] <- resample(covars[[i]], covars[["tmp_precip"]], threads = T, method = "average")
  
}

covars_stack <- rast(covars) %>% 
  tidyterra::rename("Temperature" = "tmp_precip_1",
                    "Precipitation (cm)" = "tmp_precip_2",
                    "Elevation (M)" = "elev",
                    "Forest Cover (%)" = "landcov_1",
                    "Water (%)" = "landcov_2",
                    "Wetland (%)" = "landcov_3",
                    "Physiographic Diversity" = "ergo",
                    "Cliff & Canyon (%)" = "cliff_canyon")

plot(covars_stack, col = viridis(20))

writeRaster(covars_stack, here("DataProcessed.nosync/Covariates/env_covars.tif"))
