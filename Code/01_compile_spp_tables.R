## ---------------------------
##
## Script name: 01_compile_spp_tables.R
##
## Purpose of script: Get SPP occurence by site by date
##
## Author: Trent VanHawkins
##
## Date Created: 2024-02-28
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

options(scipen = 6, digits = 5) 

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(here)
require(data.table)
require(sf)
require(terra)
require(maps)

pointlocation <- read.csv(here("DataRaw.nosync/Database/tblPointLocation.csv"), na.strings = 'NA')
acoustics <- data.table::fread(here("DataRaw.nosync/Database/tblDeploymentDetection7.csv"), na.strings = '')
deployment <- read.csv(here("DataRaw.nosync/Database/tblDeployment.csv"))

# Select Columns we need --------------------------------------------------

deployment <- deployment %>% 
  select(ID, PointLocationID)

acoustics <- acoustics %>% select(ID, DeploymentID, Night, ManualIDSpp1, ManualIDSpp2)

pointlocation <- pointlocation %>% select(ID, LocationName, Latitude, Longitude)


# Join the tables together to create one table ----------------------------

master <- left_join(acoustics, deployment, by = c("DeploymentID" = "ID")) %>% 
  ## Join to Point Location to get Lat/Long
  left_join(pointlocation, by = c("PointLocationID" = "ID")) %>% 
  ## Select the Columns we want to keep
  select(ID, LocationName, Night, Latitude, Longitude, ManualIDSpp1, ManualIDSpp2) %>% 
  ## Drop Blanks from Manual SPP ID
  drop_na(ManualIDSpp1) %>% 
  ## Fix values and ensure all in same case
  mutate(ManualIDSpp1 = case_when(ManualIDSpp1 == 'LASCIN' ~ 'LACI',
                                  ManualIDSpp1 == 'LASNOC' ~ 'LANO',
                                  ManualIDSpp1 == 'MYOCIL' ~ 'MYCI',
                                  ManualIDSpp1 == 'MYOEVO' ~ 'MYEV',
                                  ManualIDSpp1 == 'MYOLUC' ~ 'MYLU',
                                  ManualIDSpp1 == 'MYOYUM' ~ 'MYYU',
                                  ManualIDSpp1 == 'MYOCAL' ~ 'MYCA',
                                  ManualIDSpp1 == 'EPTFUS' ~ 'EPFU',
                                  ManualIDSpp1 == 'MYOTHY' ~ 'MYTH',
                                  TRUE ~ ManualIDSpp1),
         ManualIDSpp1 = tolower(ManualIDSpp1))

## Create a list of possible bat IDs
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

# Pivot Wider to get Spp Richness -------------------------------------------------------------
## Pivot Wider
master_wide <- master %>% 
  pivot_wider(names_from = ManualIDSpp1, values_from = ManualIDSpp1, 
              id_cols = c("LocationName", "Night", "Latitude", "Longitude"))

master_wide <- master_wide %>% 
  select(LocationName, Latitude, Longitude, Night, possible_bats) %>% #Select the columns we want to keep
  mutate(across(5:ncol(.), ~replace(., lengths(.) > 0, 1)), # Replace any detection with 1
         across(5:ncol(.), ~replace(., lengths(.) == 0, 0))) # Replace no detection (NULL) with 0

## Format date as year
master_wide$Night <- mdy_hms(master_wide$Night)
master_wide$year <- year(master_wide$Night)

## Remove Duplicates
master_wide <- master_wide %>% 
  distinct() %>% 
  apply(., 2, as.character)#There are no duplicates, but just to be sure

## Write out
write.table(master_wide, here("DataProcessed.nosync/SpeciesOccurrence/spp_occ_master.csv"), sep = ',')

# Plot all points to be sure ----------------------------------------------
#Us State Polygons from "maps" package
us_states <- us_map(regions = "states", include = c("OR", "WA", "ID")) %>% 
  st_transform(crs = "WGS84")

#Convert points to SF object
spp_occ <- st_as_sf(master_wide, coords = c("Longitude", "Latitude"), crs = "WGS84") 

ggplot() +
  geom_sf(data = pnw, alpha = 0.5)+
  geom_sf(data = spp_occ, aes(color = as.factor(year)))
