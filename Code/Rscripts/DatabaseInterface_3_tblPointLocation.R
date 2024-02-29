install.packages(c('tidyverse','sf','DBI','readxl','stars','elevatr'))

library(tidyverse)
library(sf)
library(DBI)
library(readxl)
library(stars)
library(nabatr)
library(foreign)
library(elevatr)

########################################################################
### RStudio uses 64 bit R by default
### MS Access and associated Drivers are 32 bit, causing mismatch errors
### Use 32 bit R for database connectivity
###   Tools > Global Options > General > R version:
### But remember to set back to 64 bit when done
########################################################################

### Establish database connection
con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/emblidgp/Desktop/PNW_BatHub_Database.accdb")

### Read in database tables
tblPointLocation <- dbReadTable(con,'tblPointLocation')
tblSiteStateCounty <- dbReadTable(con,'tblSiteStateCounty')
tblSite <- dbReadTable(con,'tblSite')
tluDatum <- dbReadTable(con,'tluDatum')
tluWaterBodyType <- dbReadTable(con,'tluWaterBodyType')
tluLocalHabitat <- dbReadTable(con,'tluLocalHabitat')

### Read in USA Census Counties data
counties <- st_read("C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/USA_Census_Counties/dtl_cnty.shp")
counties <- counties %>% 
  filter(STATE_NAME %in% c('California','Idaho','Montana','Nevada','Oregon','Utah','Washington','Wyoming')) %>% 
  select(NAME,STATE_NAME)
counties <- st_make_valid(counties)

### Read in CONUS/GRTS conversion table
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)

### Read in metadata
dat22Field <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx') %>% 
  filter(`Sample Unit` > 50) # Ignore surveys from ODFW_NorthCoastProject
dat22Paper <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx')

### Format metadata for use in tblPointLocation
dat22 <- dat22Field %>% 
  left_join(CellList, by = c('Sample Unit' = 'GRTS_ID')) %>% 
  left_join(CellList, by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(SampleUnitID = 
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         SampleUnitID_GRTS =
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         Latitude =
           case_when(is.na(Latitude) ~ y,
                     TRUE ~ Latitude),
         Longitude =
           case_when(is.na(Longitude) ~ x,
                     TRUE ~ Longitude)) %>% 
  select(State,
         SampleUnitID,
         SampleUnitID_GRTS,
         Quad,
         `Quad Number`,
         Latitude,
         Longitude,
         `Directions to Site`,
         `Habitat (choose one)`,
         `Waterbody Descriptor`,
         `Dry Water Feature Descriptor`,
         `Rock Feature Descriptor`,
         `Meadow Descriptor`,
         `Forest Edge Descriptor`,
         `Forest Opening Descriptor`,
         `Land Ownership`) %>% 
  add_row(dat22Paper %>% 
            left_join(CellList, by = c('Sample Unit' = 'GRTS_ID')) %>% 
            left_join(CellList, by = c('Sample Unit' = 'CONUS_10KM')) %>% 
            mutate(SampleUnitID = 
                     case_when(State == 'ID' ~ CONUS_10KM,
                               TRUE ~ `Sample Unit`),
                   SampleUnitID_GRTS =
                     case_when(State == 'ID' ~ `Sample Unit`,
                               TRUE ~ GRTS_ID)) %>% 
            select(State,
                   SampleUnitID,
                   SampleUnitID_GRTS,
                   Quad,
                   `Quad Number`,
                   Latitude,
                   Longitude,
                   `Directions to Site`,
                   `Habitat (choose one)`,
                   `Waterbody Descriptor`,
                   `Dry Water Feature Descriptor`,
                   `Rock Feature Descriptor`,
                   `Meadow Descriptor`,
                   `Forest Edge Descriptor`,
                   `Forest Opening Descriptor`,
                   `Land Ownership`)) %>% 
  mutate(LocationName = paste0(SampleUnitID,'_',Quad,`Quad Number`),
         LocationName_GRTS = paste0(SampleUnitID_GRTS,'_',Quad,`Quad Number`)) %>% 
  select(-c(SampleUnitID_GRTS,Quad,`Quad Number`))

### Create a table of new Station Locations (not in tblPointLocations)
tblPointLocation.new <- dat22 %>% 
  filter(!(LocationName %in% tblPointLocation$LocationName))

### Are there duplicates within year?
### If so calculate coordinates as the average
tblPointLocation.new %>% 
  group_by(LocationName) %>% 
  count %>% 
  filter(n>1)

# investigate revisits
tblPointLocation.new %>% 
  filter(LocationName=='111606_SW3')

# Calculate mean Lat, Long for revisits
tblPointLocation.new <- tblPointLocation.new %>% 
  group_by(State,SampleUnitID,LocationName,LocationName_GRTS) %>% 
  summarize(Latitude = mean(as.numeric(Latitude)),
            Longitude = mean(as.numeric(Longitude))) %>% 
  ungroup()


### Add SiteStateCountyID
tblPointLocation.new <- tblPointLocation.new %>% 
  filter(!is.na(Latitude)) %>% 
  mutate(SampleUnitID = as.character(SampleUnitID)) %>% 
  left_join(tblSite %>% 
              select(ID,SampleUnitID) %>% 
              rename('SiteID' = 'ID'),
            by = c('SampleUnitID')) %>% 
  st_as_sf(coords = c('Longitude','Latitude'),
           crs = 4269) %>% 
  st_transform(4326) %>% 
  st_join(counties) %>% 
  mutate(StateCode =
           case_when(STATE_NAME == 'California' ~ 'CA',
                     STATE_NAME == 'Idaho' ~ 'ID',
                     STATE_NAME == 'Montana' ~ 'MT',
                     STATE_NAME == 'Nevada' ~ 'NV',
                     STATE_NAME == 'Oregon' ~ 'OR',
                     STATE_NAME == 'Utah' ~ 'UT',
                     STATE_NAME == 'Washington' ~ 'WA',
                     STATE_NAME == 'Wyoming' ~ 'WY'),
         County = str_extract(NAME, '.+(?=\\sCounty)')) %>% 
  select(-NAME,-STATE_NAME) %>% 
  left_join(tblSiteStateCounty %>% 
              select(ID,SiteID,StateCode,County),
            by = c('SiteID',
                   'StateCode',
                   'County')) %>% 
  rename('SiteStateCountyID' = 'ID') %>% 
  mutate(Latitude = st_coordinates(.)[,2],
         Longitude = st_coordinates(.)[,1],
         DatumID = 1) %>% 
  st_drop_geometry() %>% 
  select(SiteID,LocationName,LocationName_GRTS,SiteStateCountyID,DatumID,Latitude,Longitude,StateCode,County)

### Add new tblSiteStateCounty records for PointLocations where the SiteStateCountyID is new
### SiteStateCountyID_new values of NA indicate there was not a matching record in tblSiteStateCounty
sitestatecounty.new <- tblPointLocation.new %>% 
  filter(is.na(SiteStateCountyID)) %>% 
  distinct(SiteID,StateCode,County) %>% 
  mutate(CreatedBy = 'P. Emblidge',
         LastModifiedBy = 'P. Emblidge')
if(dim(sitestatecounty.new)[1] > 0){
  options(odbc.batch_rows = 1)
  dbAppendTable(con,'tblSiteStateCounty',sitestatecounty.new)
}

### Check that quad number is next sequential
tmp1 <- tblPointLocation.new %>% 
  select(LocationName) %>% 
  mutate(quad = str_extract(LocationName,'.+[NS][EW]'),
         quadNum = as.numeric(str_extract(LocationName,'(?<=[EW])\\d'))) %>% 
  add_row(tblPointLocation %>% 
            select(LocationName) %>% 
            mutate(quad = str_extract(LocationName,'.+[NS][EW]'),
                   quadNum = as.numeric(str_extract(LocationName,'(?<=[EW])\\d'))))

nonSeq <- NULL
for(i in 1:length(unique(tmp1$quad))){
  tmp2 <- tmp1 %>% 
    filter(quad == unique(tmp1$quad)[i])
  a <- as.character(min(tmp2$quadNum):max(tmp2$quadNum))
  b <- as.character(sort(tmp2$quadNum))
  if(!identical(a,b)){
    nonSeq <- c(nonSeq,unique(tmp1$quad)[i])
  }}

tmp1 %>% 
  filter(quad %in% (tblPointLocation.new %>% 
           mutate(quad = str_extract(LocationName,'.+[NS][EW]')) %>% 
           filter(quad %in% nonSeq) %>% 
           pull(quad))) %>% 
  arrange(quad,quadNum)


### Elevation
tblPointLocation.new <- tblPointLocation.new %>% 
  filter(!is.na(Latitude)) %>% 
  st_as_sf(coords = c('Longitude','Latitude'),
           crs = 4269) %>% 
  st_transform(4326) %>% 
  get_elev_point() %>% # Add elevation from USGS Elevation Point Query Service
  mutate(elevation = round(elevation,digits = 0),
         Latitude = st_coordinates(.)[,2],
         Longitude = st_coordinates(.)[,1]) %>% 
  st_drop_geometry() %>% 
  rename('Elevation' = 'elevation') %>% 
  select(SiteID,LocationName,LocationName_GRTS,SiteStateCountyID,DatumID,Latitude,Longitude,Elevation)


### PrimaryAccessRoad
tblPointLocation.new <- tblPointLocation.new %>% 
  left_join(dat22 %>% 
              filter(!is.na(`Directions to Site`)) %>% 
              select(LocationName,`Directions to Site`),
            by = 'LocationName') %>% 
  rename('PrimaryAccessRoad' = 'Directions to Site')


### WaterBodyType
tblPointLocation.new <- tblPointLocation.new %>% 
  left_join(dat22 %>% 
              distinct(LocationName,`Waterbody Descriptor`),
            by = 'LocationName') %>% 
  mutate(`Waterbody Descriptor` =
           case_when(is.na(`Waterbody Descriptor`) ~ 'None',
                     `Waterbody Descriptor` == 'lake' ~ 'Lake',
                     `Waterbody Descriptor` == 'pond' ~ 'Pond',
                     `Waterbody Descriptor` == 'river' ~ 'River',
                     `Waterbody Descriptor` == 'spring' ~ 'Seep or Spring',
                     `Waterbody Descriptor` == 'stocktank/trough' ~ 'Stock Tank/Trough',
                     `Waterbody Descriptor` == 'stream' ~ 'Stream',
                     `Waterbody Descriptor` == 'wetland' ~ 'Wetland',
                     TRUE ~ 'error')) %>% 
  left_join(tluWaterBodyType,
            by = c('Waterbody Descriptor' = 'Label')) %>% 
  rename('WaterBodyTypeID' = 'ID') %>% 
  select(SiteID,LocationName,LocationName_GRTS,SiteStateCountyID,DatumID,Latitude,Longitude,Elevation,WaterBodyTypeID)

# Check that all WaterBodyTypeIDs are valid, if not (NA), revisit above code
tblPointLocation.new %>% 
  distinct(WaterBodyTypeID)


### LocalHabitatID
# Use gap_landfire_nationalterrestrialecosystems to identify ambiguous field data
tblPointLocation.new <- tblPointLocation.new %>% 
  left_join(dat22 %>% 
              distinct(LocationName,`Habitat (choose one)`),
            by = 'LocationName') %>% 
  rename('habitat' = `Habitat (choose one)`) %>% 
  mutate(habitat =
           case_when(is.na(habitat) ~ 'N/A',
                     habitat == 'agriculture' ~ 'Agriculture',
                     habitat == 'alpineforest' ~ 'Alpine Forest',
                     habitat == 'conifer' | 
                       habitat == 'mixed conifer' |
                       habitat == 'mixedconifer' |
                       LocationName == '103739_NW1' ~ 'Mixed Conifer',
                     habitat == 'dry conifer' |
                       habitat == 'dryconifer' ~ 'Dry Conifer',
                     habitat == 'grassland' |
                       LocationName == '102813_SE1' ~ 'Grassland',
                     habitat == 'hardwood' ~ 'Hardwood',
                     habitat == 'mesic forest' |
                       habitat == 'mesicforest' |
                       habitat == 'wetland' ~ 'Mesic Forest',
                     habitat == 'playa' ~ 'Dry Lake Bed',
                     habitat == 'shrub-stepp' |
                       habitat == 'shrub-steppe' ~ 'Shrub-Steppe',
                     habitat == 'urban' ~ 'Urban',
                     TRUE ~ 'error')) %>% 
  left_join(tluLocalHabitat,
            by = c('habitat' = 'Label')) %>% 
  rename('LocalHabitatID' = 'ID') %>% 
  select(SiteID,LocationName,LocationName_GRTS,SiteStateCountyID,DatumID,Latitude,Longitude,Elevation,WaterBodyTypeID,LocalHabitatID)

# Check that all LocalHabitatIDs are valid, if not (NA), revisit above code
tblPointLocation.new %>% 
  distinct(LocalHabitatID)

### Add new Local Habitats to tluLocalHabitat before adding records to tblPointLocation if needed
#dbReadTable(con,'tluLocalHabitat')
#dbExecute(con, "INSERT INTO tluLocalHabitat (Label)
#         VALUES ('Lava Flow')")


### LocationDescription
# Check that Station Locations visited more than once have the same Detection Target values, if not, correct them
dat22 %>% 
  filter(LocationName %in% tblPointLocation.new$LocationName &
           LocationName %in% (dat22 %>% 
                                group_by(LocationName) %>% 
                                count() %>% 
                                filter(n>1) %>% 
                                pull(LocationName))) %>% 
  select(LocationName,
         `Waterbody Descriptor`,
         `Dry Water Feature Descriptor`,
         `Rock Feature Descriptor`,
         `Meadow Descriptor`,
         `Forest Edge Descriptor`,
         `Forest Opening Descriptor`)
dat22 <- dat22 %>% 
  mutate(`Forest Edge Descriptor` =
           case_when(LocationName=='111606_SW3' &
                       `Forest Edge Descriptor`=='forestopenland' ~ NA_character_,
                     TRUE ~ `Forest Edge Descriptor`))

# Check that all Detection Target values in dataset are addressed in below cleanup
dat22 %>% 
  filter(LocationName %in% tblPointLocation.new$LocationName &
           !(`Waterbody Descriptor` %in% c(NA_character_,'lake','pond','river','spring','stocktank/trough','stream','wetland') |
             `Dry Water Feature Descriptor` %in% c(NA_character_,'smallarroyodrygulchbottom','small arroyo/dry gulch bottom','smallarroyodrygulchtop','small arroyo/dry gulch top','largecanyonbottom','large canyon bottom','largecanyontop','large canyon top') |
             `Rock Feature Descriptor` %in% c(NA_character_,'ridgebottom','cliffbottom','cliff bottom','cliff','ridgetop','ridge top','clifftop','cliff top') |
             str_detect(`Rock Feature Descriptor`,'[Oo]ther') |
             `Meadow Descriptor` %in% c(NA_character_,'small','medium','large') |
             `Forest Edge Descriptor` %in% c(NA_character_,'forestopenland','forest/open land','oldyoungforest','old/young forest') |
             `Forest Opening Descriptor` %in% c(NA_character_,'trail','roadway','smallgap','small gap')))

tblPointLocation.new <- tblPointLocation.new %>% 
  left_join(dat22 %>% 
              distinct(LocationName,
                       `Waterbody Descriptor`,
                       `Dry Water Feature Descriptor`,
                       `Rock Feature Descriptor`,
                       `Meadow Descriptor`,
                       `Forest Edge Descriptor`,
                       `Forest Opening Descriptor`),
            by = 'LocationName') %>% 
  mutate(`Waterbody Descriptor` =
           case_when(is.na(`Waterbody Descriptor`) ~ 'None',
                     `Waterbody Descriptor` == 'lake' ~ 'Lake',
                     `Waterbody Descriptor` == 'pond' ~ 'Pond',
                     `Waterbody Descriptor` == 'river' ~ 'River',
                     `Waterbody Descriptor` == 'spring' ~ 'Seep or Spring',
                     `Waterbody Descriptor` == 'stocktank/trough' ~ 'Stock Tank/Trough',
                     `Waterbody Descriptor` == 'stream' ~ 'Stream',
                     `Waterbody Descriptor` == 'wetland' ~ 'Wetland',
                     TRUE ~ 'error'),
         `Dry Water Feature Descriptor` =
           case_when(is.na(`Dry Water Feature Descriptor`) ~ 'None',
                     `Dry Water Feature Descriptor` == 'smallarroyodrygulchbottom' |
                       `Dry Water Feature Descriptor` == 'small arroyo/dry gulch bottom' ~ 'Small Arroyo/Dry Gulch Bottom',
                     `Dry Water Feature Descriptor` == 'smallarroyodrygulchtop' | 
                       `Dry Water Feature Descriptor` == 'small arroyo/dry gulch top'~ 'Small Arroyo/Dry Gulch Top',
                     `Dry Water Feature Descriptor` == 'largecanyonbottom' |
                       `Dry Water Feature Descriptor` == 'large canyon bottom'~ 'Large Canyon Bottom',
                     `Dry Water Feature Descriptor` == 'largecanyontop' |
                       `Dry Water Feature Descriptor` == 'large canyon top'~ 'Large Canyon Top',
                     TRUE ~ 'error'),
         `Rock Feature Descriptor` =
           case_when(is.na(`Rock Feature Descriptor`) ~ 'None',
                     `Rock Feature Descriptor` == 'ridgebottom' ~ 'Ridge Bottom',
                     `Rock Feature Descriptor` == 'cliffbottom' |
                       `Rock Feature Descriptor` == 'cliff bottom' |
                       `Rock Feature Descriptor` == 'cliff' ~ 'Cliff Bottom',
                     `Rock Feature Descriptor` == 'ridgetop' |
                       `Rock Feature Descriptor` == 'ridge top' ~ 'Ridge Top',
                     `Rock Feature Descriptor` == 'clifftop' |
                       `Rock Feature Descriptor` == 'cliff top' ~ 'Cliff Top',
                     str_detect(`Rock Feature Descriptor`,'[Oo]ther') ~ 'Other',
                     TRUE ~ 'error'),
         `Meadow Descriptor` =
           case_when(is.na(`Meadow Descriptor`) ~ 'None',
                     `Meadow Descriptor` == 'small' ~ 'Small Meadow',
                     `Meadow Descriptor` == 'medium' ~ 'Medium Meadow',
                     `Meadow Descriptor` == 'large' ~ 'Large Meadow',
                     TRUE ~ 'error'),
         `Forest Edge Descriptor` =
           case_when(is.na(`Forest Edge Descriptor`) ~ 'None',
                     `Forest Edge Descriptor` == 'forestopenland' |
                       `Forest Edge Descriptor` == 'forest/open land' ~ 'Forest/Open Land',
                     `Forest Edge Descriptor` == 'oldyoungforest' |
                       `Forest Edge Descriptor` == 'old/young forest' ~ 'Old/Young Forest',
                     TRUE ~ 'error'),
         `Forest Opening Descriptor` =
           case_when(is.na(`Forest Opening Descriptor`) ~ 'None',
                     `Forest Opening Descriptor` == 'trail' ~ 'Trail',
                     `Forest Opening Descriptor` == 'roadway' ~ 'Roadway',
                     `Forest Opening Descriptor` == 'smallgap' |
                       `Forest Opening Descriptor` == 'small gap' ~ 'Small Gap',
                     TRUE ~ 'error'))

# Check to make sure no 'error' values
tblPointLocation.new %>% 
  filter(if_any(c(`Waterbody Descriptor`,`Dry Water Feature Descriptor`,`Rock Feature Descriptor`,`Meadow Descriptor`,`Forest Edge Descriptor`,`Forest Opening Descriptor`), ~ . == 'error'))

tblPointLocation.new$LocationDescription <- NA_character_

### Build LocationDescription incrementally by each Detection Target field
tblPointLocation.new <- tblPointLocation.new %>% 
  mutate(LocationDescription = 
           case_when(`Waterbody Descriptor` == 'None' ~ '',
                     TRUE ~ paste0('Waterbody;',`Waterbody Descriptor`)))
tblPointLocation.new <- tblPointLocation.new %>% 
  mutate(LocationDescription = 
           case_when(`Dry Water Feature Descriptor` == 'None' ~
                       LocationDescription,
                     `Dry Water Feature Descriptor` != '' & LocationDescription == '' ~
                       paste0('Dry Water Feature;',`Dry Water Feature Descriptor`),
                     TRUE ~
                       paste0(str_extract(LocationDescription, '.+(?=;)'),
                              ',Dry Water Feature;',
                              str_extract(LocationDescription, '(?<=;).+'),
                              ',',
                              `Dry Water Feature Descriptor`)))
tblPointLocation.new <- tblPointLocation.new %>% 
  mutate(LocationDescription = 
           case_when(`Rock Feature Descriptor` == 'None' ~
                       LocationDescription,
                     `Rock Feature Descriptor` != '' & LocationDescription == '' ~
                       paste0('Rock Feature;',`Rock Feature Descriptor`),
                     TRUE ~
                       paste0(str_extract(LocationDescription, '.+(?=;)'),
                              ',Rock Feature;',
                              str_extract(LocationDescription, '(?<=;).+'),
                              ',',
                              `Rock Feature Descriptor`)))
tblPointLocation.new <- tblPointLocation.new %>% 
  mutate(LocationDescription = 
           case_when(`Meadow Descriptor` == 'None' ~
                       LocationDescription,
                     `Meadow Descriptor` != '' & LocationDescription == '' ~
                       paste0('Meadow;',`Meadow Descriptor`),
                     TRUE ~
                       paste0(str_extract(LocationDescription, '.+(?=;)'),
                              ',Meadow;',
                              str_extract(LocationDescription, '(?<=;).+'),
                              ',',
                              `Meadow Descriptor`)))
tblPointLocation.new <- tblPointLocation.new %>% 
  mutate(LocationDescription = 
           case_when(`Forest Edge Descriptor` == 'None' ~
                       LocationDescription,
                     `Forest Edge Descriptor` != '' & LocationDescription == '' ~
                       paste0('Forest Edge;',`Forest Edge Descriptor`),
                     TRUE ~
                       paste0(str_extract(LocationDescription, '.+(?=;)'),
                              ',Forest Edge;',
                              str_extract(LocationDescription, '(?<=;).+'),
                              ',',
                              `Forest Edge Descriptor`)))
tblPointLocation.new <- tblPointLocation.new %>% 
  mutate(LocationDescription = 
           case_when(`Forest Opening Descriptor` == 'None' ~
                       LocationDescription,
                     `Forest Opening Descriptor` != '' & LocationDescription == '' ~
                       paste0('Forest Opening;',`Forest Opening Descriptor`),
                     TRUE ~
                       paste0(str_extract(LocationDescription, '.+(?=;)'),
                              ',Forest Opening;',
                              str_extract(LocationDescription, '(?<=;).+'),
                              ',',
                              `Forest Opening Descriptor`)))

tblPointLocation.new %>% 
  distinct(LocationDescription) %>% 
  print(n=Inf)

# Add 'Other' for Station Locations with no description calculated above
tblPointLocation.new <- tblPointLocation.new %>% 
  mutate(LocationDescription =
           case_when(LocationDescription == '' ~ 'Other',
                     TRUE ~ LocationDescription))

# Remove individual detection target fields
tblPointLocation.new <- tblPointLocation.new %>% 
  select(-c(`Waterbody Descriptor`,
            `Dry Water Feature Descriptor`,
            `Rock Feature Descriptor`,
            `Meadow Descriptor`,
            `Forest Edge Descriptor`,
            `Forest Opening Descriptor`))


### Ownership
tblPointLocation %>% 
  distinct(Ownership)

# Check to see if any Land Ownership values are not currently in tblPointLocation
tblPointLocation.new %>% 
  left_join(dat22 %>% 
              distinct(LocationName,`Land Ownership`),
            by = 'LocationName') %>% 
  filter(!(`Land Ownership` %in% unique(tblPointLocation$Ownership))) %>% 
  distinct(`Land Ownership`)


tblPointLocation.new <- tblPointLocation.new %>% 
  left_join(dat22 %>% 
              distinct(LocationName,`Land Ownership`),
            by = 'LocationName') %>% 
  mutate(Ownership = 
           case_when(`Land Ownership` == 'DOE' ~ 'DOE',
                     `Land Ownership` == '' ~ NA_character_,
                     TRUE ~ `Land Ownership`)) %>% 
  select(-`Land Ownership`)

tblPointLocation.new %>% 
  distinct(Ownership)


### Add new Point Locations to tblPointLocation
names(tblPointLocation)
names(tblPointLocation.new)

tblPointLocation.new <- tblPointLocation.new %>% 
  mutate(CreatedBy = 'P. Emblidge',
         LastModifiedBy = 'P. Emblidge')

### Clean up decimal places
tblPointLocation.new$Longitude <- format(round(as.numeric(tblPointLocation.new$Longitude),7), nsmall = 7)
tblPointLocation.new$Latitude <- format(round(as.numeric(tblPointLocation.new$Latitude),7), nsmall = 7)
tblPointLocation.new$Elevation <- round(as.numeric(tblPointLocation.new$Elevation))

### Make sure there are no '' values in tblPointLocation.new
tblPointLocation.new %>% 
  filter_all(any_vars(. == ''))

options(odbc.batch_rows = 1)
dbAppendTable(con,'tblPointLocation',tblPointLocation.new)


### It's always a good idea to close database connections at the end of your session
#odbcCloseAll()
dbDisconnect(con)

###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
### Check existing tblPointLocation records ###
tblPointLocation <- dbReadTable(con,'tblPointLocation')
tblSiteStateCounty <- dbReadTable(con,'tblSiteStateCounty')

# Create a data.frame of tblPointLocation records all transformed to CRS Geographic WGS84
tblPointLocation.sf1 <- st_as_sf(tblPointLocation %>% 
                                   filter(DatumID == 1),
                                 coords = c('Longitude','Latitude'))
st_crs(tblPointLocation.sf1) <- 4269
tblPointLocation.sf1 <- st_transform(tblPointLocation.sf1,4326)

tblPointLocation.sf2 <- st_as_sf(tblPointLocation %>% 
                                   filter(DatumID == 2),
                                 coords = c('Longitude','Latitude'))
st_crs(tblPointLocation.sf2) <- 4267
tblPointLocation.sf2 <- st_transform(tblPointLocation.sf2,4326)

tblPointLocation.sf3 <- st_as_sf(tblPointLocation %>% 
                                   filter(DatumID == 3),
                                 coords = c('Longitude','Latitude'))
st_crs(tblPointLocation.sf3) <- 4326

tblPointLocation.sf <- rbind(tblPointLocation.sf1,
                             tblPointLocation.sf2,
                             tblPointLocation.sf3)

### Identify tblPointLocation records with SiteStateCountyID values that do not match the current method
tblPointLocation.sf %>% 
  select(SiteID,LocationName,SiteStateCountyID) %>% 
  st_join(counties) %>% 
  mutate(StateCode =
           case_when(STATE_NAME == 'California' ~ 'CA',
                     STATE_NAME == 'Idaho' ~ 'ID',
                     STATE_NAME == 'Montana' ~ 'MT',
                     STATE_NAME == 'Nevada' ~ 'NV',
                     STATE_NAME == 'Oregon' ~ 'OR',
                     STATE_NAME == 'Utah' ~ 'UT',
                     STATE_NAME == 'Washington' ~ 'WA',
                     STATE_NAME == 'Wyoming' ~ 'WY'),
         County = str_extract(NAME, '.+(?=\\sCounty)')) %>% 
  select(-NAME,-STATE_NAME) %>% 
  left_join(tblSiteStateCounty %>% 
              select(ID,SiteID,StateCode,County),
            by = c('SiteID',
                   'StateCode',
                   'County')) %>% 
  rename('SiteStateCountyID_new' = 'ID') %>% 
  filter(SiteStateCountyID != SiteStateCountyID_new |
           is.na(SiteStateCountyID_new))

# 114381_NW4 coordinates are within a coastal river area that falls outside of county boundaries, it's SiteStateCountyID is good


### Elevation check
# Add elevation from USGS Elevation Point Query Service
tmp1 <- get_elev_point(tblPointLocation.sf)

# Compare tblPointLocation Elevation values with those obtained from USGS Elevation Point Query Service
tmp1 %>% 
  mutate(ElevCheck = abs(elevation - Elevation),
         ElevCheck2 = Elevation/elevation) %>% 
  select(ID,LocationName,Elevation,elevation,ElevCheck,ElevCheck2) %>% 
  arrange(desc(ElevCheck2))


### Look at StationLocation Names that don't have full sequence of PointLocations
tblPointLocation %>% 
  mutate(Cell = str_extract(LocationName,'[^_]+'),
         Quad = str_extract(LocationName,'(?<=_).{2}'),
         QuadNo = str_extract(LocationName,'(?<=[EW])\\d+')) %>% 
  select(Cell,Quad,QuadNo) %>% 
  group_by(Cell,Quad) %>% 
  summarize(n = length(QuadNo),
            max = max(QuadNo)) %>% 
  filter(n != max)

tblPointLocation %>% 
  filter(str_detect(LocationName,'100044_SW'))


### It's always a good idea to close database connections at the end of your session
#odbcCloseAll()
dbDisconnect(con)
