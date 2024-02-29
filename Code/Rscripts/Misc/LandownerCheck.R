### Land Ownership Check
library(tidyverse)
library(sf)
library(DBI)
library(lubridate)
library(readxl)

#########################################################
### Read in previous detector locations from database ###
#########################################################
con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/emblidgp/Downloads/NPSBats_BE_1_7_OR_CA_WA_ID_Complete_20220718.accdb")

tblSite <- dbReadTable(con,'tblSite')
tblPointLocation <- dbReadTable(con, 'tblPointLocation')
tblDeployment <- dbReadTable(con,'tblDeployment')


### OSU cells
cells <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2022.xlsx', sheet = '2022', col_types = 'text')
head(cells)
cells <- cells %>% 
  filter(Agency=='OSU') %>% 
  pull(CONUS_10KM)


### Survey Locations of Cells managed by OSU
tmp1 <- tblPointLocation %>% 
  left_join(tblSite,by = c('SiteID' = 'ID')) %>% 
  select(ID,SiteID,LocationName,LocationName_GRTS,Latitude,Longitude,Ownership,SampleUnitID) %>% 
  filter(SampleUnitID %in% cells)

#write_csv(tmp1,'C:/Users/emblidgp/Desktop/tblPointLocation_OSU.csv')


### OSU Cell deployments 2016 - 2022
dat <- tblDeployment %>% 
  select(PointLocationID,DeploymentDate) %>% 
  left_join(tblPointLocation %>%
              select(ID,SiteID,LocationName,LocationName_GRTS,Latitude,Longitude,PrimaryAccessRoad,Ownership), 
            by = c('PointLocationID' = 'ID')) %>% 
  filter(str_extract(LocationName,'\\d+') %in% cells) %>% 
  mutate(Year = format(DeploymentDate,'%Y'),
         Surveyed = T) %>% 
  distinct(LocationName,Year,Surveyed) 

dat20 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/2020_Analysis/2020 NABat PNW Metadata (Collector & Paper 20210622).xlsx',sheet = 'Metadata') %>%
  mutate(LocationName = paste0(CONUS_10KM,'_',QuadID,Quad_No),
         Year = '2020',
         Surveyed = T) %>% 
  filter(str_extract(LocationName,'\\d+') %in% cells) %>% 
  distinct(LocationName,Year,Surveyed)


dat21 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/2021_Analysis/NABatMetadata2021.xlsx',sheet = 'NABatMetadata') %>% 
  filter(!is.na(`To BatHub Server`) & `To BatHub Server` != 'NoSurvey') 

CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)

dat21 <- dat21 %>% 
  left_join(CellList, by = c('Sample Unit' = 'GRTS_ID')) %>% 
  left_join(CellList, by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(SampleUnitID = 
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         LocationName = paste0(SampleUnitID, '_', Quad, `Quad Number`),
         Year = '2021',
         Surveyed = T) %>% 
  filter(str_extract(LocationName,'\\d+') %in% cells) %>% 
  distinct(LocationName,Year,Surveyed)


dat22 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/2022_Analysis/NABatSurveyForm2022_20220902.csv') %>% 
  left_join(CellList, by = c('Sample Unit' = 'GRTS_ID')) %>% 
  left_join(CellList, by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(SampleUnitID = 
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         LocationName = paste0(SampleUnitID, '_', Quad, `Quad Number`),
         Year = '2022',
         Surveyed = T) %>% 
  filter(str_extract(LocationName,'\\d+') %in% cells) %>% 
  distinct(LocationName,Year,Surveyed)


sites <- dat %>% 
  add_row(dat20) %>% 
  add_row(dat21) %>% 
  add_row(dat22) %>% 
  pivot_wider(id_cols = LocationName,names_from = Year,names_sort = T,values_from = Surveyed)

sites



### My priority is checking on locations likely to be surveyed again


### Read in file of tblPointLocation records of OSU-managed Cells with PADUS info joined
full <- read_csv('C:/Users/emblidgp/Desktop/tblPointLocation_OSU_PADUS_onX.csv')

sites <- sites %>% 
  left_join(full %>% 
              select(LocationName,Latitude,Longitude,Ownership,PADUS_Match,Loc_Own,Loc_Mang,Unit_Nm,Pub_Access,NEAR_DIST,onX),
            by = 'LocationName')

head(sites)

sites %>% 
  filter(PADUS_Match == F &
           (!is.na(`2021`) |
              !is.na(`2022`))) %>% 
  select(LocationName,Ownership,13:18)


### The most detail is stored in the LandownerContact.xlsx file at the moment, 
### show records that do not have a value in that table
contact <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Contacts/LandownerContacts.xlsx')

contact <- contact %>% 
  mutate(LocationName = 
           case_when(is.na(LocationName) & str_detect(LocationName_GRTS,'[NS][EW]\\d') ~ 
                       paste0(CONUS_10KM,'_',str_extract(LocationName_GRTS,'[NS][EW]\\d')),
                     TRUE ~ LocationName))

sites %>% 
  filter(PADUS_Match == F &
           (!is.na(`2021`) |
              !is.na(`2022`)) &
           !(LocationName %in% contact$LocationName)) %>% 
  arrange(LocationName)
