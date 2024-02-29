library(tidyverse)
library(stringr)
library(lubridate)
library(DBI)
library(readxl)


### Historic Station Locations (2016 - YYYY) ####

# Connect to Access database
con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/emblidgp/Downloads/PNW_BatHub_Database.accdb")

# Check which data is in database
dbReadTable(con,'tblDeployment') %>% 
  summarize(min(DeploymentDate),max(DeploymentDate))

# Read in 2016 - 2020 from Access Database
tblPointLocation <- dbReadTable(con,'tblPointLocation') %>% 
  left_join(dbReadTable(con,'tblSite'),
            by = c('SiteID' = 'ID')) %>% 
  mutate(LocationName_GRTS = paste0(SampleUnitID_GRTS,str_extract(LocationName,'_[NS][EW]\\d'))) %>% 
  select(LocationName,LocationName_GRTS,Latitude,Longitude)

# Read in 2021 from metadata
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)

dat21 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2021_Analysis/NABatMetadata2021.xlsx')

dat21 <- dat21 %>% 
  filter(!str_detect(`Deployment Date`,'2020') &
           `Sample Unit` != '' &
           `To BatHub Server` != 'NoSurvey') %>% 
  mutate(Latitude =
           case_when(is.na(Latitude) &
                       y != 0 ~ y,
                     TRUE ~ Latitude),
         Longitude =
           case_when(is.na(Longitude) &
                       x != 0 ~ x,
                     TRUE ~ Longitude)) %>% 
  filter(!is.na(Latitude)) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'GRTS_ID')) %>% 
  mutate(LocationName = 
           case_when(State == 'ID' ~ paste0(CONUS_10KM,'_',Quad,`Quad Number`),
                     TRUE ~ paste0(`Sample Unit`,'_',Quad,`Quad Number`)),
         LocationName_GRTS = 
           case_when(State == 'ID' ~ paste0(`Sample Unit`,'_',Quad,`Quad Number`),
                     TRUE ~ paste0(GRTS_ID,'_',Quad,`Quad Number`))) %>% 
  group_by(LocationName,LocationName_GRTS) %>% 
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude)) %>% 
  filter(!(LocationName %in% tblPointLocation$LocationName)) %>% 
  ungroup()


### Combine and save
dat <- rbind(tblPointLocation,dat21)

celltracker <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2023.xlsx") %>% 
  mutate(GRTS_ID = as.character(GRTS_ID)) %>% 
  select(GRTS_ID,NABatProject,Agency)

dat <- dat %>% 
  mutate(GRTS_ID = str_extract(LocationName_GRTS,'\\d+')) %>% 
  left_join(celltracker) %>% 
  select(-GRTS_ID)

write_csv(dat,'C:/Users/emblidgp/Desktop/SiteStatus2023/NABatSurveys2016_2021.csv')


### Sites surveyed previous season ####
# Historic Station Locations
dat <- read_csv("C:/Users/emblidgp/Desktop/SiteStatus2023/NABatSurveys2016_2021.csv")

meta1 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx")
meta2 <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx")

dat22 <- meta1 %>% 
  select(State,`Sample Unit`,Quad,`Quad Number`,x,y) %>% 
  rename("Latitude" = "y",
         "Longitude" = "x") %>% 
  add_row(meta2 %>% 
            select(State,`Sample Unit`,Quad,`Quad Number`,Longitude,Latitude)) %>% 
  # Remove those ODFW NorthCoast Project sites
  filter(!(State == 'OR' &
             `Sample Unit` %in% c(20,30,33,34,37,41,44,45)))

dat22 <- dat22 %>% 
  filter(!is.na(Latitude)) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'GRTS_ID')) %>% 
  mutate(Site_CONUS = 
           case_when(State == 'ID' ~ paste0(CONUS_10KM,'_',Quad,`Quad Number`),
                     TRUE ~ paste0(`Sample Unit`,'_',Quad,`Quad Number`)),
         Site_GRTS = 
           case_when(State == 'ID' ~ paste0(`Sample Unit`,'_',Quad,`Quad Number`),
                     TRUE ~ paste0(GRTS_ID,'_',Quad,`Quad Number`))) %>% 
  group_by(State,Site_CONUS,Site_GRTS) %>% 
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude)) %>% 
  ungroup()


### Read in Species Richness
SpRich1 <- read_csv("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/SppRichness_PNW_2022.csv")
SpRich2 <- read_csv("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/SppRichness_USFWS_2022.csv") %>% 
  select(-`FWS Refuge`)
SpRich <- SpRich1 %>% 
  add_row(SpRich2) %>% 
  mutate(Site_GRTS = paste0(GRTS,str_extract(Site,'_[NS][EW]\\d'))) %>% 
  group_by(Site_GRTS) %>% 
  summarize(across(ANPA:TABR,~if(sum(.) > 0){1}else{0})) %>% 
  mutate(BatSpp2022 = select(.,ANPA:TABR) %>% rowSums()) %>% 
  select(Site_GRTS,BatSpp2022)


### Add Species Richness from previous season, identify if last season was the first for Station Location, add assessment columns
dat22 <- dat22 %>% 
  left_join(SpRich, by = 'Site_GRTS') %>% 
  mutate(New2022 = 
           case_when(Site_CONUS %in% dat$LocationName ~ 'No',
                     TRUE ~ 'Yes'),
         Status2023 = 
           case_when(New2022 == "Yes" &
                       BatSpp2022 < 3 ~ '',
                     TRUE ~ 'Resurvey'),
         StatusNote = '')


### Format metadata for including in next year's Field Maps
meta <- meta1 %>% 
  mutate(`Detector Serial No.` = as.character(`Detector Serial No.`)) %>% 
  select(1:40) %>% 
  add_row(meta2 %>% 
            select(1:20,22,34:51)) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'GRTS_ID')) %>% 
  mutate(Site_CONUS = 
           case_when(State == 'ID' ~ paste0(CONUS_10KM,'_',Quad,`Quad Number`),
                     TRUE ~ paste0(`Sample Unit`,'_',Quad,`Quad Number`)),
         Site_GRTS = 
           case_when(State == 'ID' ~ paste0(`Sample Unit`,'_',Quad,`Quad Number`),
                     TRUE ~ paste0(GRTS_ID,'_',Quad,`Quad Number`))) %>% 
  select(Site_CONUS,
         Site_GRTS,
         `Directions to Site`,
         `Site Name`,
         `Land Ownership`,
         `Enter other Land Ownership`,
         `Land Owner Address`,
         `Land Owner Email`,
         `Land Owner Phone`,
         `Microphone Orientation`,
         `Distance to Clutter (m)`,
         `Clutter Category`,
         `Clutter Type`,
         `Enter Other Clutter Type`,
         `Photo Direction`,
         `Habitat (choose one)`,
         `Waterbody Descriptor`,
         `Dry Water Feature Descriptor`,
         `Rock Feature Descriptor`,
         `Meadow Descriptor`,
         `Forest Edge Descriptor`,
         `Forest Opening Descriptor`,
         `Other Detection Target`,
         Comments)

names(meta) <- c('Site_CONUS','Site_GRTS','Directions','SiteName','Landowner','Landowner2','OwnerAddr','OwnerEmail',
                 'OwnerPhone','MicDir','CltrDist','CltrClass','CltrType','CltrType2','PhotoDir','Habitat','Waterbody',
                 'DryWater','Rock','Meadow','ForestEdge','ForestOpen','DetectTarg','Comments')


### Any SiteStatus records without metadata?
dat22 %>% 
  filter(!(Site_CONUS %in% meta$Site_CONUS))

### Any metadata without SiteStatus records
meta %>% 
  filter(!(Site_CONUS %in% dat22$Site_CONUS))

### Removed Flight's End records from SiteStatus because extra site for the cell. 
### 394_SW3 removed from SiteStatus because it was a 5th site that performed the poorest
### 4138_SE2 removed from SiteStatus because poor performance
### 4138_SE3 removed from SiteStatus because poor performance
### 5018_NE1 removed from SiteStatus because it was an extra site that performed poorly
### 5018_SE1 removed from SiteStatus because it was an extra site that performed poorly


### Consolidating revisits to single site record, keeping the record with most complete metadata
meta <- meta %>% 
  arrange(rowSums(meta == '')) %>%            # Sort by number of blank values, fewer occurring first
  filter(!duplicated(Site_CONUS))       # filter out duplicate sites


### Create a single file with all Sites for 2022 and including as much metadata as is possible
dat22 <- dat22 %>% 
  left_join(meta, by = c('Site_CONUS','Site_GRTS')) %>% 
  mutate(GRTS_ID = str_extract(Site_GRTS,"\\d+")) %>% 
  left_join(celltracker, by = c('GRTS_ID')) %>% 
  select(-c('GRTS_ID','Agency'))

write_csv(dat22,'C:/Users/emblidgp/Desktop/SiteStatus2023/NABatSiteStatus2023.csv')