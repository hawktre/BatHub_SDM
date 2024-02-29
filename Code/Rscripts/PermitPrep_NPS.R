### This script pulls all NABat Station Locations surveyed since 2016 and finds the ones that occur on NPS lands

library(tidyverse)
library(lubridate)
library(DBI)
library(readxl)

### Start with R version 4.1.2 (32-bit)
### Connect to Access database
con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/emblidgp/Downloads/PNW_BatHub_Database.accdb")


#################################
### Previously surveyed sites ###
#################################
### 2016 - 2020 from Access Database
tblSite <- dbReadTable(con,'tblSite')
tblPointLocation <- dbReadTable(con,'tblPointLocation')
tblDeployment <- dbReadTable(con,'tblDeployment')

dat <- tblDeployment %>% 
  left_join(tblPointLocation,
            by = c('PointLocationID' = 'ID')) %>% 
  left_join(tblSite,
            by = c('SiteID' = 'ID')) %>% 
  mutate(Year = as.numeric(str_extract(DeploymentDate,'\\d{4}')),
         SampleUnitID = as.numeric(SampleUnitID),
         SampleUnitID_GRTS = as.numeric(SampleUnitID_GRTS)) %>% 
  rename('GRTS' = 'SampleUnitID_GRTS',
         'CONUS' = 'SampleUnitID') %>% 
  select(GRTS,CONUS,LocationName,LocationName_GRTS,Latitude,Longitude,Year)



### Sites surveyed in 2021
dat2021 <- read_excel('C:/Users/emblidgp/Desktop/NABatMetadata2021.xlsx')

CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)

dat2021 <- dat2021 %>% 
  filter(!str_detect(`Deployment Date`,'2020') &
           `Sample Unit` != '') %>% 
  mutate(Latitude =
           case_when(is.na(Latitude) &
                       y != 0 ~ y,
                     TRUE ~ Latitude),
         Longitude =
           case_when(is.na(Longitude) &
                       x != 0 ~ x,
                     TRUE ~ Longitude)) %>% 
  filter(!is.na(Latitude)) %>% 
  select(State,`Sample Unit`,Quad,`Quad Number`,Latitude,Longitude,`Deployment Date`) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'GRTS_ID')) %>% 
  mutate(GRTS =
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         CONUS =
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         LocationName = paste0(CONUS,'_',Quad,`Quad Number`),
         LocationName_GRTS = paste0(GRTS,'_',Quad,`Quad Number`)) %>% 
  select(CONUS,GRTS,LocationName,LocationName_GRTS,Latitude,Longitude,`Deployment Date`) %>% 
  group_by(CONUS,GRTS,LocationName,LocationName_GRTS) %>% 
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude)) %>% 
  mutate(Year = 2021)

# Remove those Flight's End surveys in 117633
dat2021 <- dat2021 %>% 
  filter(!(LocationName %in% c('117633_NW3','117633_NW4','117633_NW5','117633_NW6')))


dat <- dat %>% 
  add_row(dat2021)


### Sites surveyed 2022
meta1 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx")
meta2 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx")

dat2022 <- meta1 %>% 
  select(State,`Sample Unit`,Quad,`Quad Number`,`Deployment Date`,x,y) %>% 
  rename("Latitude" = "y",
         "Longitude" = "x") %>% 
  add_row(meta2 %>% 
            select(State,`Sample Unit`,Quad,`Quad Number`,`Deployment Date`,Longitude,Latitude))

dat2022 <- dat2022 %>% 
  filter(!is.na(Latitude)) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'GRTS_ID')) %>% 
  mutate(GRTS =
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         CONUS =
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         LocationName = paste0(CONUS,'_',Quad,`Quad Number`),
         LocationName_GRTS = paste0(GRTS,'_',Quad,`Quad Number`)) %>% 
  select(CONUS,GRTS,LocationName,LocationName_GRTS,Latitude,Longitude,`Deployment Date`) %>% 
  group_by(CONUS,GRTS,LocationName,LocationName_GRTS) %>% 
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude)) %>% 
  mutate(Year = 2022) %>% 
  ungroup()

# Remove those Flight's End surveys in 117633 and the ODFW NorthCoast Project
dat2022 <- dat2022 %>% 
  filter(!(LocationName %in% c('117633_NW4','117633_NW6','117633_NW7','117633_NW8')) &
           !CONUS %in% c(20,30,33,34,37,41,44,45))

dat <- dat %>% 
  add_row(dat2022)

write_csv(dat,"C:/Users/emblidgp/Desktop/StationLocations_2016_2022.csv")

### It's always a good idea to close database connections at the end of your session
dbDisconnect(con)


### Switch to R version 4.2.2 (64-bit)
library(tidyverse)
library(arcgisbinding)
library(sf)
library(sfheaders)
library(readxl)

### Read in Station Locations
dat <- read_csv("C:/Users/emblidgp/Desktop/StationLocations_2016_2022.csv")

dat <- dat %>% 
  st_as_sf(coords = c('Longitude','Latitude'))
st_crs(dat) <- 4269
dat <- st_transform(dat,4326)


### Read in CellTracker
CellTracker <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2023.xlsx")


### Read in NPS lands
# PAD-US 3.0 Fee Manager
# Pad-US does not fully document federal lands in JODA
arc.check_product()
admin <- arc.open("https://services.arcgis.com/v01gqwM5QqNysAAi/arcgis/rest/services/Fee_Manager/FeatureServer/0")
select_statement <- "State_Nm IN ('OR','WA','ID') AND Loc_Mang = 'NPS'"
admin <- arc.select(admin,where_clause = select_statement)
admin <- arc.data2sf(admin)
admin <- st_make_valid(admin)
admin <- st_transform(admin,4326)

# Station Locations on NPS lands
dat_admin <- dat %>% 
  st_join(admin, join = st_intersects) %>% 
  filter(!is.na(OBJECTID))

# Get mean location for Station Locations with multiple records
dat_admin <- dat_admin %>% 
  sf_to_df(fill = T) %>% 
  group_by(GRTS,CONUS,LocationName,LocationName_GRTS,State_Nm,Loc_Nm) %>% 
  summarize(Latitude = mean(y),
            Longitude = mean(x)) %>% 
  arrange(State_Nm,Loc_Nm,GRTS,LocationName_GRTS) %>% 
  ungroup()

# NPS Station Locations in SUs that BatHub is responsible for
dat_admin %>% 
  left_join(CellTracker %>% 
              select(GRTS_ID,Agency),
            by = c('GRTS' = 'GRTS_ID')) %>% 
  filter(Agency == 'OSU') %>% 
  arrange(GRTS,LocationName) %>% 
  mutate(across(everything(),as.character))


# NPS Tracts from NPS Official Service-wide Datasets
nps <- arc.open("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/NPS_Land_Resources_Division_Boundary_and_Tract_Data_Service/FeatureServer/1")
select_statement <- "STATENAME IN ('OR','WA','ID') AND STATUS <> 'PRIV'"
nps <- arc.select(nps,where_clause = select_statement)
nps <- arc.data2sf(nps)
nps <- st_make_valid(nps)
nps <- st_transform(nps,4326)

### Station Locations on NPS lands
#sf_use_s2(FALSE)
dat_nps <- dat %>% 
  st_join(nps, join = st_intersects) %>% 
  filter(!is.na(OBJECTID))
#sf_use_s2(TRUE)

# Get mean location for Station Locations with multiple records
dat_nps <- dat_nps %>% 
  sf_to_df(fill = T) %>% 
  group_by(GRTS,CONUS,LocationName,LocationName_GRTS,STATENAME,ALPHA) %>% 
  summarize(Latitude = mean(y),
            Longitude = mean(x)) %>% 
  arrange(STATENAME,ALPHA,GRTS,LocationName_GRTS) %>% 
  ungroup()

# NPS Station Locations in SUs that BatHub is responsible for
dat_nps %>% 
  left_join(CellTracker %>% 
              select(GRTS_ID,Agency),
            by = c('GRTS' = 'GRTS_ID')) %>% 
  filter(Agency == 'OSU') %>% 
  arrange(GRTS,LocationName) %>% 
  mutate(across(everything(),as.character))


write_csv(dat,"C:/Users/emblidgp/Desktop/NPS_StationLocations_2016_2022.csv")



