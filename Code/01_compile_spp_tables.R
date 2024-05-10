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
require(usmap)

pointlocation <- read.csv(here("DataRaw.nosync/Database/tblPointLocation.csv"), na.strings = 'NA')
acoustics <- data.table::fread(here("DataRaw.nosync/Database/tblDeploymentDetection7.csv"), na.strings = '')
deployment <- read.csv(here("DataRaw.nosync/Database/tblDeployment.csv"))
wbwg <- readxl::read_xlsx(here("Background/SpeciesNatHistMatrix.xlsx"), sheet = 2) %>% 
  drop_na(fourlettername)

# Select Columns we need --------------------------------------------------

deployment <- deployment %>% 
  select(ID, PointLocationID)

acoustics <- acoustics %>% select(ID, DeploymentID, Night, ManualIDSpp1, ManualIDSpp2)

pointlocation <- pointlocation %>% select(ID, LocationName, Latitude, Longitude)

wbwg <- wbwg %>% select(fourlettername, `SUM ROOST`, SPECIES)
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


# Remove WA TABR ----------------------------------------------------------

##find the record
bad_tabr <-master %>% filter(ManualIDSpp1 == "tabr") %>% dplyr::slice_max(Latitude) %>% .$ID

##remove bad record
master <- master %>% filter(ID != bad_tabr)

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

master_wide <- as.data.frame(apply(master_wide, 2, unlist))

## Remove Duplicates
master_wide <- master_wide %>% 
  distinct() 

#There are no duplicates, but just to be sure
master_wide %>% 
  group_by(LocationName, year) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

## For sites with multiple nights, spp was observed if observed on ANY night
master_wide <- master_wide %>% 
  group_by(LocationName, year) %>% 
  summarise(Latitude = max(Latitude),
            Longitude = max(Longitude),
            laci = max(laci),
            lano = max(lano),
            myev = max(myev),
            epfu = max(epfu),
            myyu = max(myyu),
            myth = max(myth),
            myci = max(myci),
            myvo = max(myvo),
            tabr = max(tabr),
            anpa = max(anpa),
            pahe = max(pahe),
            euma = max(euma),
            myca = max(myca),
            mylu = max(mylu),
            coto = max(coto),
            year = max(year)) %>% 
  ungroup()


## Format as character 
master_wide$year <- as.character(master_wide$year)



## Write out
write_csv(master_wide, here("DataProcessed.nosync/SpeciesOccurrence/spp_occ_master.csv"))

# Plot all points to be sure ----------------------------------------------
us_states <- us_map(regions = "states", include = c("OR", "WA", "ID")) %>% 
  st_transform(crs = 'WGS84')
#Us State Polygons from "maps" package
pnw <- us_map(regions = "states", include = c("OR", "WA", "ID")) %>% 
  st_transform(crs = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83')

#Add buffer and union
pnw <- pnw %>% 
  st_make_valid() %>% 
  st_buffer(10000) %>% 
  st_union() %>% 
  st_transform(crs = "WGS84") %>% 
  st_sf() %>% 
  st_cast()

#Pivot Longer
master_long <- pivot_longer(master_wide, cols = 5:ncol(master_wide), names_to = "spp", values_to = "presence") %>% 
  filter(presence > 0) %>% 
  left_join(., wbwg, by = c("spp" = "fourlettername"))

#Convert points to SF object
spp_occ <- st_as_sf(master_long, coords = c("Longitude", "Latitude"), crs = "WGS84") 

spp_occ <- st_crop(spp_occ, pnw)
# Create Plots ------------------------------------------------------------
plot.occs <- function(bg, spp.occ, spp.group, pal){
  ggplot() +
    geom_sf(data = us_states)+
    geom_sf(data = spp.occ %>% filter(`SUM ROOST` == spp.group), aes(col = spp))+
    facet_wrap(~SPECIES, ncol = 3) +
    scale_color_brewer(palette = pal)+
    labs(color = "Year",
         title = "Species Occurrences 2016-2022")+
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())
}

## Overall Plot
overall.occs <- ggplot() +
  geom_sf(data = us_states)+
  geom_sf(data = spp_occ, aes(col = year))+
  facet_wrap(~year, ncol = 3) +
  labs(color = "Year",
       title = "Species Occurrences 2016-2022")+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
ggsave(filename = "overall_occs.png", plot = overall.occs, path = here("Reports/SDM_Presentation/Figures"), width = 3024, height = 1964, units = "px")
## Generalists
general.occs <- plot.occs(us_states, spp_occ, spp.group = "GENERAL", pal = "Set1")
ggsave(filename = "general_occs.png", plot = general.occs, path = here("Reports/SDM_Presentation/Figures"), width = 3024, height = 1964, units = "px")

## Cliff/Cave
cliff.occs <- plot.occs(us_states, spp_occ, spp.group = "CLIFF", pal = "Set2")
ggsave(filename = "cliffcanyon_occs.png", plot = cliff.occs, path = here("Reports/SDM_Presentation/Figures"), width = 3024, height = 1964, units = "px")

## Trees
trees.occs <- plot.occs(us_states, spp_occ, spp.group = "TREES", pal = "Set3")
ggsave(filename = "trees_occs.png", plot = trees.occs, path = here("Reports/SDM_Presentation/Figures"), width = 3024, height = 1964, units = "px")

## Other
other.occs <- plot.occs(us_states, spp_occ, spp.group = "OTHER", pal = "Accent")
ggsave(filename = "other_occs.png", plot = other.occs,path = here("Reports/SDM_Presentation/Figures"), width = 3024, height = 1964, units = "px")

