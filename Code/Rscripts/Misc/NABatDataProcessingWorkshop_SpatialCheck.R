rm(list = ls())
### Install and load packages ####
install.packages(c('devtools','sp','sf','tidyverse','DBI','lubridate','readxl'))
devtools::install_github('usgs/nabatr', build_vignettes = TRUE, upgrade = 'never', force = TRUE)

lapply(c('devtools','sp','sf','tidyverse','DBI','lubridate','readxl','nabatr','units'),require,character.only = T)


#### Load and Format prior NABat Station Locations ####

##### Pull locations from NABat #####
username = 'patrick.emblidge@oregonstate.edu'
token = get_nabat_gql_token(username)

# Get projects lookup table
project_df <- get_projects(token)

# Identify project(s) to pull data from
project_id <- project_df %>% 
  filter(project_name %in% c("NABat Idaho",
                             "NW NABat",
                             "National Wildlife Refuges Pacific Northwest Region 1 (R1)",
                             "Forest Service Region 4 NABat-NW Bat Hub")) %>% 
  pull(project_id)

# Pull lat/long for all locations in all our projects
NABat_locations <- NULL
for(i in 1:length(project_id)){
  sa_survey_df <- get_sa_project_summary(token,
                                         project_df,
                                         project_id[i])
  tmp1 <- get_sa_event_metadata(token,
                                sa_survey_df,
                                year = 'all') %>% 
    distinct(grts_cell_id,
             location_name,
             latitude,
             longitude)
  NABat_locations <- rbind(NABat_locations,tmp1)}

# Save NABat Locations
write_csv(NABat_locations,"C:/Users/emblidgp/Desktop/NABatDataProcessingWorkshop/NABat_Locations.csv")


##### Pull locations from Access database and format to match NABat #####
# For projects that don't include coordinates of Surveys. 
# For data that hasn't been uploaded to NABat yet. 

# Need to use 32-bit R to interact with Access database

lapply(c('devtools','sp','sf','tidyverse','DBI','lubridate','readxl','nabatr'),require,character.only = T)

con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/emblidgp/Downloads/PNW_BatHub_Database.accdb")
tblPointLocation <- dbReadTable(con, 'tblPointLocation') %>% 
  left_join(dbReadTable(con,'tblSite'), by = c('SiteID' = 'ID')) %>% 
  mutate(grts_cell_id = SampleUnitID_GRTS,
         location_name =
           case_when(ParkCode == 'ID' ~ LocationName_GRTS,
                     TRUE ~ LocationName)) %>% 
  select(grts_cell_id,location_name,Latitude,Longitude) %>% 
  rename('latitude' = 'Latitude',
         'longitude' = 'Longitude')

# It's always a good idea to close database connections at the end of your session
dbDisconnect(con)

# Save as CSV file
write_csv(tblPointLocation,"C:/Users/emblidgp/Desktop/NABatDataProcessingWorkshop/tblPointLocation.csv")


##### Pull locations from CSV file #####
tblPointLocation <- read_csv("C:/Users/emblidgp/Desktop/NABatDataProcessingWorkshop/tblPointLocation.csv")


##### Combine all prior Station Locations into a single file #####
StationLocations <- NABat_locations %>% 
  filter(!is.na(latitude)) %>% 
  add_row(tblPointLocation %>% 
            filter(location_name %!in% (NABat_locations %>% 
                                          filter(!is.na(latitude)) %>% 
                                          pull(location_name))))

write_csv(StationLocations,"C:/Users/emblidgp/Desktop/NABatDataProcessingWorkshop/NABat_StationLocations_existing.csv")


##### Convert existing Station Locations to simple features #####
StationLocations <- StationLocations %>% 
  st_as_sf(coords = c('longitude','latitude'))
st_crs(StationLocations) <- 4326


#### Load and format Metadata from current season ####
meta1 <- read_excel('C:/Users/emblidgp/Desktop/NABatDataProcessingWorkshop/NABatMetadata2022_FieldMaps.xlsx')
meta2 <- read_excel('C:/Users/emblidgp/Desktop/NABatDataProcessingWorkshop/NABatMetadata2022_PaperDatasheets.xlsx')

# Combine digital and paper metadata
meta <- meta1 %>% 
  select(State,`Sample Unit`,Quad,`Quad Number`,x,y) %>% 
  add_row(meta2 %>% 
            select(State,`Sample Unit`,Quad,`Quad Number`,Longitude,Latitude) %>% 
            rename(x = Longitude,
                   y = Latitude))

# Remove records with missing coordinates
meta <- meta %>% 
  filter(!is.na(x))

# Format metadata for Spatial Checks
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)

CellTracker <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2023.xlsx")

meta <- meta %>% 
  left_join(CellList, by = c('Sample Unit' = 'GRTS_ID')) %>% 
  left_join(CellList, by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(CONUS = 
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         GRTS =
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         LocationNameGRTS = paste0(GRTS, '_', Quad, `Quad Number`),
         LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`)) %>% 
  left_join(CellTracker,by = c('GRTS_ID')) %>% 
  mutate(location_name = 
           case_when(NABatProject == 'NW' ~ LocationNameCONUS,
                     TRUE ~ LocationNameGRTS)) %>% 
  select(GRTS,location_name,x,y) %>% 
  rename('grts_cell_id' = 'GRTS',
         'longitude' = 'x',
         'latitude' = 'y')

write_csv(meta,"C:/Users/emblidgp/Desktop/NABatDataProcessingWorkshop/NABat_StationLocations_2022.csv")

# Convert to simple features
meta <- st_as_sf(meta,
                 coords = c('longitude','latitude'))
st_crs(meta) <- 4269
# Transform to WGS84
meta <- st_transform(meta, 4326)


#### Spatial Checks ####

# Read in existing Station Locations
StationLocations <- read_csv("C:/Users/emblidgp/Desktop/NABatDataProcessingWorkshop/NABat_StationLocations_existing.csv")

# Convert to simple features
StationLocations <- StationLocations %>% 
  st_as_sf(coords = c('longitude','latitude'))
st_crs(StationLocations) <- 4326


# Read in current Station Locations
meta <- read_csv("C:/Users/emblidgp/Desktop/NABatDataProcessingWorkshop/NABat_StationLocations_2022.csv")

# Convert to simple features
meta <- st_as_sf(meta,
                 coords = c('longitude','latitude'))
st_crs(meta) <- 4269
# Transform to WGS84
meta <- st_transform(meta, 4326)


##### Do Station Locations occur within the intended Sample Unit? #####
# Pull CONUS 10KM SU polygons from NABat
# Create sp object (SpatialPolygonsDataFrame) of attributed Sample Units
SUquery <- paste0("GRTS_ID IN ('",
                 paste(meta %>% 
                         distinct(grts_cell_id) %>% 
                         pull(),
                       collapse = "','"),
                 "')")

SUs <- get_grts_data('Conus',query = SUquery)

# Convert Sample Units to simple features object
SUs <- st_as_sf(SUs)

# Transform to WGS84
SUs <- st_transform(SUs, 4326)

meta %>% 
  st_join(SUs) %>%
  filter(is.na(GRTS_ID) | 
                 grts_cell_id != GRTS_ID) %>% 
  {if(length(.$grts_cell_id) != 0){
    stop(paste0('Station Location(s) ',paste(.$location_name,collapse = ', '),' does not occur within the intended Sample Unit.'))}}


##### Do Station Locations occur within the intended Quad? #####
# Pull CONUS 5KM Quads from shapefile
# This layer was created in ArcGIS Pro, Subdivide Polygon (Data Management Tools), Number of Equal Areas = 4, Subdivision Type = Stacked Blocks
quads <- st_read('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABatGrid5kRegionIntercardinal/NABat5kGrid_Region_shp/NABatGrid5k_Region.shp')

# Transform to WGS84
quads <- st_transform(quads, 4326)


meta %>% 
  st_join(quads) %>% 
  filter(str_extract(location_name,'[NS][EW]') != InterCardi) %>% 
  {if(length(.$grts_cell_id) != 0){
    stop(paste0('Station Location(s) ',paste(.$location_name,collapse = ', '),' does not occur within the intended Sample Unit Quad.'))}}


##### Check distance between Stations surveyed multiple times in current year #####
tmp1 <- data.frame(site = 
                     meta %>%
                     group_by(location_name) %>%
                     count() %>%
                     filter(n > 1) %>% 
                     pull(location_name),
                   dist = NA)

for(i in 1:length(tmp1$site)){
  tmp2 <- meta %>% 
    filter(location_name == tmp1$site[i])
  tmp1$dist[i] = max(st_distance(tmp2))
}

tmp1


##### Check distance between current Station locations and existing locations #####
a <- StationLocations %>% 
  add_row(meta) %>% 
  group_by(location_name) %>% 
  count() %>% 
  filter(n>1) %>% 
  select(location_name) %>% 
  data.frame(dist=NA)

for(i in 1:length(a$location_name)){
  tmp1 <- StationLocations %>% 
    filter(location_name == a$location_name[i])
  tmp2 <- meta %>% 
    filter(location_name == a$location_name[i])
  a$dist[i] <- max(st_distance(tmp1,tmp2))
}


a %>% 
  filter(dist >50) %>% 
  arrange(location_name)


##### Check that all new sites are next in sequential order #####
all <- meta %>% 
  add_row(StationLocations) %>% 
  distinct(location_name) %>% 
  mutate(quad = str_extract(location_name,'\\d+_[NS][EW]'),
         quadNo = as.numeric(str_extract(location_name,'(?<=_[NS][EW])\\d')),
         Sequential = NA_character_) %>% 
  arrange(location_name)

for(i in 1:length(all$quad)){
  tmp1 <- all %>% 
    filter(quad == all$quad[i]) %>% 
    pull(quadNo)
  if(length(unique(diff(c(0,tmp1)))) == 1 &
     unique(diff(c(0,tmp1)))[1] == 1){
    all$Sequential[i] <- 'Yes'}else{
      all$Sequential[i] <- 'No'}
  }

# Quads with non-sequential Station Locations
paste0("The following Quads have non-sequential Station Locations: ", paste(all %>% 
                                                                         filter(Sequential == 'No') %>% 
                                                                           pull(quad),
                                                                         collapse = ', '))

# Station Locations that are non-sequential
all %>% 
  filter(quad %in% (all %>% 
                      filter(Sequential == 'No') %>% 
                      pull(quad)))


##### Overlapping surveys with different LocationName #####
all <- StationLocations %>% 
  add_row(meta %>% 
            filter(location_name %!in% StationLocations$location_name))


# Nearest Neighbor of all existing Station Locations
nearest_all <- all %>% 
  mutate(nearest_index = st_nearest_feature(.),
         nearest = all$location_name[nearest_index],
         dist = st_distance(.,all[nearest_index,],by_element = T)) %>% 
  select(-nearest_index)

# Pull all new Station Locations with an existing location nearby
nearest_all %>% 
  filter(dist < set_units(100,m)) %>% 
  arrange(dist) %>% 
  print(n=Inf)
