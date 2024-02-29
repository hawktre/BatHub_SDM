### Submit results for all NABat surveys conducted on WA DNR lands prior to requesting new survey year permit.
library(tidyverse)
library(readxl)
library(sf)
library(arcgisbinding)
library(sfheaders)

### Station Locations ####
# Pull from metadata
meta1 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx")
meta2 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx")

meta <- meta1 %>% 
  select(State,`Sample Unit`,Quad,`Quad Number`,`Deployment Date`,`Land Ownership`,`Deployment Contact(s)`,`Deployment Agency`,`Habitat (choose one)`,`Waterbody Descriptor`,`Dry Water Feature Descriptor`,`Rock Feature Descriptor`,`Meadow Descriptor`,`Forest Opening Descriptor`,`Forest Edge Descriptor`,x,y,Altitude) %>% 
  rename("Latitude" = "y",
         "Longitude" = "x") %>% 
  add_row(meta2 %>% 
            select(State,`Sample Unit`,Quad,`Quad Number`,`Deployment Date`,`Land Ownership`,`Deployment Contact(s)`,`Deployment Agency`,`Habitat (choose one)`,`Waterbody Descriptor`,`Dry Water Feature Descriptor`,`Rock Feature Descriptor`,`Meadow Descriptor`,`Forest Opening Descriptor`,`Forest Edge Descriptor`,Longitude,Latitude,`Altitude (m)`) %>% 
            rename('Altitude' = 'Altitude (m)'))

meta.sf <- meta %>% 
  filter(!is.na(Latitude)) %>% 
  st_as_sf(coords = c('Longitude','Latitude'))
st_crs(meta.sf) <- 4269
meta.sf <- st_transform(meta.sf,4326)


### Read in spatial data ####
# We compared PAD-US 3.0 with WA DNR Parcel layer, and DNR layer matches onX better

# Read in WA DNR Parcel layer downloaded from WA DNR GIS Open Data Portal
arc.check_product()
WADNR <- arc.open("https://services1.arcgis.com/CD5mKowwN6nIaqd8/arcgis/rest/services/WA_DNR_Managed_Land_Parcels/FeatureServer/0")
WADNR <- arc.select(WADNR)
WADNR <- arc.data2sf(WADNR)
WADNR <- st_make_valid(WADNR)
WADNR <- st_transform(WADNR,4326)

# Read in DNR Regions
DNR_Regions <- arc.open("https://services1.arcgis.com/CD5mKowwN6nIaqd8/arcgis/rest/services/WA_DNR_Regions/FeatureServer/0")
DNR_Regions <- arc.select(DNR_Regions)
DNR_Regions <- arc.data2sf(DNR_Regions)
DNR_Regions <- st_make_valid(DNR_Regions)
DNR_Regions <- st_transform(DNR_Regions,4326)

# Read in USA Counties
#counties <- arc.open("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_Counties/FeatureServer/0")
#select_statement <- "STATE_NAME IN ('Oregon','Washington')"
#counties <- arc.select(counties, where_clause = select_statement)
#counties <- arc.data2sf(counties)
#counties <- st_make_valid(counties)
#counties <- st_transform(counties,4326)


### Join spatial data to metadata ####
meta.sf <- meta.sf %>% 
  st_join(DNR_Regions %>% 
            select(JURISDIC_2), join = st_intersects) %>% 
  st_join(WADNR %>% 
            select(FID), join = st_intersects) %>% 
#  st_join(counties %>% 
#            select(NAME), join = st_intersects) %>% 
  filter(!is.na(FID)) %>% 
  select(-FID) %>% 
  distinct() %>% 
  mutate(StationLocation = paste0(`Sample Unit`, '_', Quad, `Quad Number`),
         `Deployment Date` = as.Date(`Deployment Date`),
         DetectionTarget =
           case_when(`Forest Opening Descriptor` != '' ~ paste0('ForestOpening; ', `Forest Opening Descriptor`),
                     `Forest Edge Descriptor` != '' ~ paste0('ForestEdge; ', `Forest Edge Descriptor`),
                     `Meadow Descriptor` != '' ~ paste0('Meadow; ', `Meadow Descriptor`),
                     `Rock Feature Descriptor` != '' ~ paste0('RockFeature; ', `Rock Feature Descriptor`),
                     `Dry Water Feature Descriptor` != '' ~ paste0('DryWater; ', `Dry Water Feature Descriptor`),
                     `Waterbody Descriptor` != '' ~ paste0('Waterbody; ', `Waterbody Descriptor`),
                     TRUE ~ ''))


### Clean up habitat
meta.sf %>% 
  distinct(`Habitat (choose one)`)
meta.sf <- meta.sf %>% 
  mutate('Habitat' =
           case_when(`Habitat (choose one)` == 'dryconifer' ~ 'dry conifer',
                     `Habitat (choose one)` == 'mixedconifer' ~ 'mixed conifer',
                     `Habitat (choose one)` == 'mesicforest' ~ 'mesic forest',
                     `Habitat (choose one)` == 'shrub-stepp' ~ 'shrub-steppe',
                     TRUE ~ `Habitat (choose one)`))
meta.sf %>% 
  distinct(Habitat)


### Format metadata
meta <- meta.sf %>% 
  st_transform(4269) %>% 
  sf_to_df(fill = TRUE) %>% 
  mutate(Datum = 'NAD83',
         Altitude = round(Altitude,digits = 0)) %>% 
  select(JURISDIC_2,StationLocation,`Deployment Date`,Datum,x,y,Altitude,`Deployment Agency`,Habitat,DetectionTarget) %>% 
  rename('DNR Region' = 'JURISDIC_2',
         'Longitude' = 'x',
         'Latitude' = 'y',
         'Organization' = 'Deployment Agency',
         'Elevation' = 'Altitude') %>% 
  arrange(`DNR Region`,StationLocation)


# Read in species richness data
SpRich1 <- read_csv("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/SppRichness_PNW_2022.csv")
SpRich2 <- read_csv("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/SppRichness_USFWS_2022.csv")
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)
SpRich2 <- SpRich2 %>% 
  left_join(CellList,by = c('GRTS' = 'GRTS_ID')) %>% 
  mutate(Site = str_replace(Site,'\\d+',as.character(CONUS_10KM))) %>% 
  select(-c(CONUS_10KM,`FWS Refuge`))

SpRich <- SpRich1 %>% 
  add_row(SpRich2)


# Join Species Richness to Metadata
data <- meta %>% 
  left_join(SpRich, by = c('StationLocation' = 'Site')) %>% 
  select(-c(GRTS,SiteDeployment))

# Pivot longer
data2 <- data %>% 
  pivot_longer(ANPA:TABR, names_to = 'SpeciesCode') %>% 
  filter(value == 1)

# Read in Bat List
bats <- read.csv('C:/Users/emblidgp/Desktop/BatList.csv')

# Add species name
data2 <- data2 %>% 
  left_join(bats, by = c('SpeciesCode' = 'Code')) %>% 
  distinct() %>% 
  select(-c(SpeciesCode,UnusualOccurrences,value))


head(data2)
write_csv(data, 'C:/Users/emblidgp/Box/HERS_Working/Bats/DataRequests/WA_DNR/DataRequest_NABat_WADNR_2022_Surveys.csv')
write_csv(data2, 'C:/Users/emblidgp/Box/HERS_Working/Bats/DataRequests/WA_DNR/DataRequest_NABat_WADNR_2022_Species.csv')

data %>% 
  distinct(str_extract(StationLocation,'\\d+'))

data %>% 
  distinct(`DNR Region`)
