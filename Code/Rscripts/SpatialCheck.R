### This script is a preliminary QC step for checking the Station Location spatial components

# Install and load packages ----
if(!'pacman' %in% installed.packages()){
  install.packages('pacman')
}

pacman::p_load(tidyverse,
               readxl,
               lubridate,
               devtools,
               sp,
               sf,
               odbc,
               DBI,
               units)

# Identify yourself and year of data to check ----
onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")

# Pull locations from NW Bat Hub database ----
# for NW NABat project (because we don't upload coordinates for this project to NABat)
# Need to use 32-bit R to interact with Access database
con <- dbConnect(odbc::odbc(),
                 .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/",onid,"/Desktop/PNW_BatHub_Database.accdb"))
tblPointLocation <- dbReadTable(con, 'tblPointLocation') %>% 
  select(LocationName, Longitude, Latitude) %>% 
  rename('LocationNameCONUS' = 'LocationName')

# Convert to simple features
tblPointLocation.sf <- st_as_sf(tblPointLocation, coords = c('Longitude','Latitude'))
st_crs(tblPointLocation.sf) <- 4269

# Transform to WGS84
tblPointLocation.sf <- st_transform(tblPointLocation.sf, 4326)


# Read in CONUS 10KM SU polygons from shapefile on Box ----
SUs <- st_read(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.shp'))

# Transform to WGS84
SUs <- st_transform(SUs, 4326)

# Read in CONUS 5KM Quads ----
# This layer was created in ArcGIS Pro, Subdivide Polygon (Data Management Tools), Number of Equal Areas = 4, Subdivision Type = Stacked Blocks
quads <- st_read(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/GIS/NABatGrid5kRegionIntercardinal/NABat5kGrid_Region_shp/NABatGrid5k_Region.shp'))
quads <- st_transform(quads,4269)

# Transform to WGS84
quads <- st_transform(quads, 4326)


# Read in and format Metadata ----
if(file.exists(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_FieldMaps.xlsx'))){
  meta1 <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_FieldMaps.xlsx'))
}else{
  meta1 <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatSurveyForm',year,'.csv'))
}
meta2 <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_PaperDatasheets.xlsx'))

# Combine digital and paper metadata
meta <- meta1 %>% 
  select(State,CONUS,GRTS,Quad,`Quad Number`,'Longitude' = x,'Latitude' = y) %>% 
  add_row(meta2 %>% 
            select(State,CONUS,GRTS,Quad,`Quad Number`,Longitude,Latitude)) %>% 
  filter(!CONUS %in% c(116234,116697)) %>% # 116234 and 116697 Deployments are ODFW NorthCoast Project, not NABat
  filter(!GRTS %in% c(1949,107692)) # 1949 and 107692 are USFS Region04 SUs that we do not currently maintain in our database

# Check any missing coordinates
meta %>% 
  filter(is.na(Longitude))

# Remove records with missing coordinates
meta <- meta %>% 
  filter(!is.na(Longitude))

# Check for erroneous coordinates
min(meta$Longitude)
max(meta$Longitude)
min(meta$Latitude)
max(meta$Latitude)

# Use SUs to add CONUS and GRTS column for each station
meta <- meta %>% 
  mutate(LocationNameGRTS = paste0(GRTS, '_', Quad, `Quad Number`),
         LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`)) %>% 
  select(CONUS,GRTS,LocationNameCONUS,LocationNameGRTS,Longitude,Latitude)

# Convert to simple features
# NOTE missing coordinate values are not allowed, so only StationLocations with coordinates can be converted to simple features
meta.sf <- st_as_sf(meta,
                    coords = c('Longitude','Latitude'))
st_crs(meta.sf) <- 4269
# Transform to WGS84
meta.sf <- st_transform(meta.sf, 4326)


# Spatial Checks ----
## Do Points fall withing the intended SU? ----
meta.sf %>% 
  st_join(SUs) %>% 
  filter(GRTS != GRTS_ID) %>% 
  print(n=Inf)

meta.sf %>% 
  filter(is.na(GRTS))


## Do Points fall withing the intended Quad? ----
meta.sf %>% 
  st_join(quads) %>% 
  filter(str_extract(LocationNameCONUS,'\\d+_[NS][EW]') != paste0(CONUS_10KM,'_',InterCardi)) %>% 
  print(n=Inf)

### 126901 does not have SW quad shapefile record
### 96809_NE2 is in NW Quad, but < 50 m from database location NE2
### 125970_NW1 is in NE Quad, but < 50 m from database location NW1
### 131531 does not have quad shapefile record
### 114381_SW2 is in NW Quad, but < 50 m from existing location SW2


## Check distance between Stations surveyed multiple times in 2023 ----
tmp1 <- data.frame(site = 
                     meta %>%
                     group_by(LocationNameCONUS) %>%
                     count() %>%
                     filter(n > 1) %>% 
                     pull(LocationNameCONUS),
                   dist = NA)

for(i in 1:length(tmp1$site)){
  tmp2 <- meta.sf %>% 
    filter(LocationNameCONUS == tmp1$site[i])
  tmp1$dist[i] = max(st_distance(tmp2))
}

tmp1


## Check distance between 2023 Station locations and database locations ----
a <- tblPointLocation %>% 
  add_row(meta %>% 
            select(LocationNameCONUS,Latitude,Longitude)) %>% 
  group_by(LocationNameCONUS) %>% 
  count() %>% 
  filter(n>1) %>% 
  select(LocationNameCONUS) %>% 
  data.frame(dist=NA)

for(i in 1:length(a$LocationNameCONUS)){
  tmp1 <- tblPointLocation.sf %>% 
    filter(LocationNameCONUS == a$LocationNameCONUS[i])
  tmp2 <- meta.sf %>% 
    filter(LocationNameCONUS == a$LocationNameCONUS[i])
  a$dist[i] <- max(st_distance(tmp1,tmp2))
}

# Display all locations with discrepancies
# Add notes here for all locations reviewed and determined to be okay, so there's no need to review each time this script is run
a %>% 
  filter(dist > 100 &
           LocationNameCONUS != "112066_SE1" & # Photo coordinates are <50m from 2022 survey location, same detection target
           LocationNameCONUS != "103755_NE1" & # No deployment
           LocationNameCONUS != "124109_NW2" & # < 50m from 2022 survey location
           LocationNameCONUS != "115786_NE1" & # < 50m from 2022 survey location
           LocationNameCONUS != "119480_SE5" & # < 50m from 2022 survey location
           LocationNameCONUS != "100495_SW5" & # < 50m from 2022 survey location
           LocationNameCONUS != "104255_SW1" & # LocationName and coordinates are fine, same detection target
           LocationNameCONUS != "100990_NW4" & # LocationName and coordinates are fine, same detection target
           LocationNameCONUS != "124109_NE1" & # < 50m from 2022 survey location
           LocationNameCONUS != "128760_NW1" & # < 50m from 2022 survey location
           LocationNameCONUS != "107000_SE1" & # LocationName and coordinates are fine, same detection target
           LocationNameCONUS != "102353_SE1" & # LocationName and coordinates are fine, same detection target
           LocationNameCONUS != "110232_SW1" & # LocationName and coordinates are fine, same detection target
           LocationNameCONUS != "123182_NE2") %>% # LocationName and coordinates are fine, biologist confirmed that this is not a new detection target
  arrange(desc(dist))


## Check that all new sites are next in sequential order ----
new <- meta %>% 
  filter(!(LocationNameCONUS %in% tblPointLocation$LocationNameCONUS)) %>% 
  mutate(QuadNum = as.numeric(str_extract(LocationNameCONUS, '(?<=_[NS][EW])\\d')),
         last = NA_integer_)

for(i in 1:length(new$LocationNameCONUS)){
  tmp1 <- str_extract(new$LocationNameCONUS[i], '\\d+_[NS][EW]')
  tmp2 <- tblPointLocation %>% 
    filter(str_detect(LocationNameCONUS, tmp1))
  if(length(tmp2$LocationNameCONUS) == 0){
    new$last[i] <- 0
  }else{
    new$last[i] <- as.numeric(max(str_extract(tmp2$LocationNameCONUS, '(?<=_[NS][EW])\\d')))
  }
}

# Display all locations with discrepancies
# Add notes here for all locations reviewed and determined to be okay, so there's no need to review each time this script is run
new %>% 
  filter(QuadNum - last != 1 &
           !CONUS %in% c(116234,116697) &         # ODFW NorthCoast project
           LocationNameGRTS != "586_SW2" &        # SW1 and SW2 surveyed in 2022
           LocationNameGRTS != "689_NE3" &        # Surveyed in 2022
           LocationNameGRTS != "689_NE5" &        # Surveyed in 2022
           LocationNameGRTS != "730_NE2" &        # USFWS surveyed in 2022
           LocationNameCONUS != "115350_NW4" &    # New, NW1 - NW3 are all existing locations
           LocationNameCONUS != "102370_NW3" &    # New, NW1 - NW2 are all existing locations
           LocationNameCONUS != "117182_NE1" &    # NE1 and NE2 surveyed in 2022, but clock error resulted in no acoustic data for NE1, so it's not in NABat dataset
           LocationNameCONUS != "90852_NE3" &     # NE2 and NE3 new in 2023
           LocationNameCONUS != "113519_NW3" &    # NW2 and NW3 new in 2023
           LocationNameCONUS != "120911_NW4" &    # NW3 and NW4 new in 2023
           LocationNameCONUS != "94526_SE5" &     # SE4, SE5, and SE6 new in 2023
           LocationNameCONUS != "94526_SE6" &     # SE4, SE5, and SE6 new in 2023
           LocationNameCONUS != "104648_NW1" &    # Only existing Location prior to 2023 was NW2
           LocationNameCONUS != "93629_NE3" &     # NE2 and NE3 new in 2023
           LocationNameCONUS != "117169_NE4" &    # NE3 and NE4 new in 2023
           LocationNameCONUS != "117169_SE6") %>% # SE5 and SE6 new in 2023
           arrange(GRTS,LocationNameCONUS) %>% 
  print(n=Inf)


## Overlapping surveys with different LocationName ----
all <- tblPointLocation.sf %>% 
  add_row(meta.sf %>% 
            filter(!LocationNameCONUS %in% tblPointLocation$LocationNameCONUS) %>% 
            select(LocationNameCONUS))

nearest_all <- all %>% 
  mutate(nearest_index = st_nearest_feature(.),
         nearest = all$LocationNameCONUS[nearest_index],
         dist = st_distance(.,all[nearest_index,],by_element = T)) %>% 
  select(-nearest_index)

# Pull all Station Locations with an existing location nearby
nearest_all %>% 
  filter(dist < set_units(50,m) &
           !LocationNameCONUS %in% c("112077_NE1","112077_NE2",      # Existing locations (NE1 and NE2) are 5m apart
                                     "127360_SE2","127360_SE3",      # Existing locations (SE2 and SE3) are 10m apart
                                     "97323_NE1","97323_NE3",        # Distinct detection targets 24m apart
                                     "97323_NW1","97323_NW2",        # Distinct detection targets 31m apart
                                     "97323_SW2","97323_SW3",        # Distinct detection targets 31m apart
                                     "96809_NW1","96809_NW2",        # Existing locations (NW1 and NW2) are 25m apart
                                     "115352_NW1","115352_NW2",      # Existing locations (NW1 and NW2) are 28m apart
                                     "120917_NW2","120917_NW3",      # Existing locations (NW2 and NW3) are 48m apart, different private properties
                                     "102366_NW2","102366_NW3",      # Existing locations (NW2 and NW3) are 50m apart
                                     "107020_SE1","107020_SE2",      # Existing locations (SE1 and SE2) are 50m apart
                                     "125086_SE2","125087_SW1",      # 125087_SW1 is an accidental location outside of intended SU which was surveyed in the past
                                     "115350_NW2","115350_NW3") &    # Distinct detection targets 16m apart
           !str_detect(LocationNameCONUS,"116234|116697")) %>%       # 116234 and 116697 Deployments are ODFW NorthCoast Project, not NABat
  arrange(dist) %>% 
  print(n=Inf)
