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

options(scipen = 6, digits = 10) 

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(here)
require(terra)
require(tidyterra)
require(sf)
require(raptr)
require(janitor)
require(ENMeval)
require(ecospat)

# Read in Data ------------------------------------------------------------
spp_occ <- read_csv(here("DataProcessed.nosync/SpeciesOccurrence/spp_occ_master.csv"))
covars <- rast(here("DataProcessed.nosync/Covariates/env_covars.tif"))
pnw <- st_read(here("DataProcessed.nosync/Covariates/pnw_buff.gpkg"))


# Drop ergo landforms from covars -----------------------------------------

covars <- covars %>% select(-"Physiographic Diversity")

# Create results vector for each species -------------------------------
## Define all bats
possible_bats <- c("laci",
                   "lano",
                   "myev",
                   "epfu",
                   "myyu",
                   "myth",
                   "myci",
                   "myvo",
                   "tabr",
                   "anpa",
                   "pahe",
                   "euma",
                   "myca",
                   "mylu",
                   "coto")
## Create an empty vector to store results
spp_list <- list()

## Create the outcome for each species
for (i in possible_bats) {
  tmp_names <- c("Latitude", "Longitude", i)
  
  tmp <- spp_occ[,names(spp_occ) %in% tmp_names]
  
  spp_list[[i]] <- tmp %>%
    filter(tmp[i] > 0) %>% 
    select(-c(i))
}

# Convert spp_occ to sf and extract points --------------------------------------------

spat_extract <- function(occs){
  ## Converting spp_occ data frame to spatial object
  spp_spat <- st_as_sf(occs, coords = c("Longitude", "Latitude"), crs = "WGS84")

  ## Crop out Northern CA Points
  spp_crop <- st_crop(spp_spat, pnw) %>% st_coordinates() %>% as.data.frame()
  
  # Extract environemental covariates 
  spp_covs <- bind_cols(spp_crop, terra::extract(covars, spp_crop, bind = T) %>% as.data.frame())
  
  return(spp_covs)
}

## Extract covariate values for all spp matrices and clean up names
spp_mats <- lapply(spp_list, spat_extract) %>% lapply(., clean_names)

saveRDS(spp_mats, here("DataProcessed.nosync/SpeciesOccurrence/spp_occ_mats.rds"))
## Remove Tabr not enough samples
spp_mats <- spp_mats[names(spp_mats) != "tabr"]

# Background Points -------------------------------------------------------
## We are going to do something with a random component, so we set the seed for reproducibility
set.seed(123)

## sample background points and convert to sf object
bg <- st_as_sf(randomPoints(covars, n = 10000) %>% as.data.frame(), coords = c('x', 'y'), crs = "WGS84")

## extract covariate values for those points
envs.bg <- terra::extract(covars, bg, bind = T) %>% st_as_sf() 

## Create dataframe
envs.bg.df <- bind_cols(st_coordinates(envs.bg), as.data.frame(envs.bg)) %>% clean_names() %>% select(-geometry)


# Partitioning ------------------------------------------------------------

kfold.partitions <- lapply(spp_mats, function(x) {get.randomkfold(occs = x, bg = envs.bg.df, kfolds = 5)})

block.partitions <- lapply(spp_mats, function(x) {get.block(occs = x, bg = envs.bg.df, orientation = "lat_lon")})


# Make a function for running maxent --------------------------------------

maxnet.fit <- function(occs, envs, bg, partitions){
  
  #Select only lat and long for points
  occs <- occs %>% select(x, y)
  bg <- bg %>% select(x, y)

  #Convert covariates to raster stack
  covars_stack <- raster::stack(envs)
  
  print(names(covars_stack))


  #run maxnet
  
  res <- ENMevaluate(occs = occs, envs = covars_stack, bg = bg,
              partitions = "user", user.grp = partitions,
              tune.args = list(fc = c("L", "LQ", "LQH"), rm = 1:5),
              algorithm = "maxnet", parallel = T)

  return(res)
  
}


# Run enmeval  ------------------------------------------------------------

all.res <- mapply(function(x, y){maxnet.fit(occs = x, envs = covars, bg = envs.bg.df, partitions = y)},
                  x = spp_mats, y = kfold.partitions)

block.res <- mapply(function(x, y){maxnet.fit(occs = x, envs = covars, bg = envs.bg.df, partitions = y)},
                    x = spp_mats, y = block.partitions)

# Save results ------------------------------------------------------------
saveRDS(all.res, here("DataProcessed.nosync/ModResults/all_res.rds"))
saveRDS(block.res, here("DataProcessed.nosync/ModResults/block_res.rds"))
 