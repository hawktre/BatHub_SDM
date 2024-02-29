library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(lutz)

### Read in full metadata----
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)

## FieldMaps----
meta1 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx')

meta1 <- meta1 %>% 
  mutate(zone = tz_lookup_coords(y,x,method = 'accurate'),
         `Deployment Date` =
           case_when(zone == 'America/Boise' ~
                       format(with_tz(`Deployment Date`, tzone = 'America/Boise'),'%Y-%m-%d'),
                     zone == 'America/Los_Angeles' ~
                       format(with_tz(`Deployment Date`, tzone = 'America/Los_Angeles'),'%Y-%m-%d')))

meta1 <- meta1 %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'GRTS_ID')) %>%
  mutate('GRTS' = 
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         'CONUS' = 
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         'Location Name' = paste0(`Sample Unit`,'_',Quad,`Quad Number`)) %>% 
  select(State,GRTS,CONUS,`Location Name`,`Deployment Date`,`Land Ownership`,`Detector Type`,`Microphone Height (m)`,x,y) %>% 
  rename('Latitude' = 'y',
         'Longitude' = 'x')

## Paper Datasheets----
meta2 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx')

meta2 <- meta2 %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  left_join(CellList,
            by = c('Sample Unit' = 'GRTS_ID')) %>%
  mutate('GRTS' = 
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         'CONUS' = 
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         'Location Name' = paste0(`Sample Unit`,'_',Quad,`Quad Number`),
         `Deployment Date` = as.character(`Deployment Date`)) %>% 
  select(State,GRTS,CONUS,`Location Name`,`Deployment Date`,`Land Ownership`,`Detector Type`,`Microphone Height (m)`,Longitude,Latitude)

## USFS Region04 metadata----
meta3 <- read_excel('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/FS R4 Call Analysis/2022 NABat Data/USFS_Region4_Metadata_2022.xlsx')

meta3 <- meta3 %>% 
  left_join(CellList,
            by = c('GRTS Cell Id' = 'GRTS_ID')) %>% 
  mutate(`Deployment Date` = as.character(`Deployment Date`)) %>% 
  filter(`Location Name` != '3802_NE1') %>% # Remove survey on BLM lands with no acoustic data provided
  select(State,`GRTS Cell Id`,CONUS_10KM,`Location Name`,`Deployment Date`,`Land Ownership`,`Detector Type`,`Microphone Height (m)`,Longitude,Latitude) %>% 
  rename('GRTS' = 'GRTS Cell Id',
         'CONUS' = 'CONUS_10KM')

meta <- meta1 %>% 
  add_row(meta2) %>% 
  add_row(meta3)

### Remove non-NABat records
# ODFW-Atwood Units
meta <- meta %>% 
  filter(!(State == 'OR' &
             CONUS %in% c(20,30,33,34,37,41,44,45)))

write_csv(meta %>% 
            filter(!is.na(Latitude)) %>% 
            mutate(across(everything(),as.character)),
          'C:/Users/emblidgp/Box/HERS_Working/Bats/DataRequests/BatAMP_DataRequest_2022/NABatMetadata2022.csv')

### In ArcGIS Pro (Project BatAMP_Prep), join federal land manager to metadata


### Read in surveys conducted on USFS (all states) or BLM (OR/WA) managed lands
meta2 <- read.csv('C:/Users/emblidgp/Box/HERS_Working/Bats/DataRequests/BatAMP_DataRequest_2022/NABatMetadata2022_FederalLands.csv')

### There are lots of discrepancies between Land Ownership field in metadata and actual ownership at coordinates
### Best to use coordinates to join federal management agency

### Subset metadata based on GIS calculated ownership
meta <- meta %>% 
  filter(`Location Name` %in% meta2$Location_Name)

meta %>% 
  distinct(`Location Name`)
dim(meta)


### General Headings. Required for all uploads.
###   first_name: Data owner or representative's first name
meta <- meta %>% 
  mutate(first_name = 'Beth')
###   last_name: Data owner or representative's last name
meta <- meta %>% 
  mutate(last_name = 'Ward')
###   y_coord: Northing coordinate (decimal degree latitude or UTM N)
meta <- meta %>% 
  mutate(y_coord = Latitude)
###   x_coord: Easting coordinate (decimal degree longitude or UTM E)
meta <- meta %>% 
  mutate(x_coord = Longitude)
###   det_mfg: Dropdown menu for detector manufacturer selection.
meta %>%
  distinct(`Detector Type`)
meta <- meta %>% 
  mutate(det_mfg = 
           case_when(`Detector Type` == 'D500X' | 
                       `Detector Type` == 'D500x' ~ 'Pettersson',
                     `Detector Type` == 'SM4BAT' |
                       `Detector Type` == 'SM3BAT' |
                       `Detector Type` == 'Song Meter Mini Bat' ~ 'Wildlife Acoustics',
                     `Detector Type` == 'Anabat Express' ~ 'Anabat',
                     `Detector Type` == 'AudioMoth' ~ 'AudioMoth',
                     TRUE ~ 'check'))
meta %>%
  distinct(det_mfg)
###   det_model: Dropdown menu for detector model selection.
meta %>%
  distinct(`Detector Type`)
meta <- meta %>% 
  mutate(det_model = 
           case_when(`Detector Type` == 'D500X' | 
                       `Detector Type` == 'D500x' ~ 'D500X',
                     `Detector Type` == 'SM4BAT' ~ 'SM4BAT-FS',
                     `Detector Type` == 'SM3BAT' ~ 'SM3BAT',
                     `Detector Type` == 'Song Meter Mini Bat' ~ 'MINI BAT',
                     `Detector Type` == 'Anabat Express' ~ 'Anabat Express',
                     `Detector Type` == 'AudioMoth' ~ 'AudioMoth 1.2',
                     TRUE ~ 'check'))
meta %>%
  distinct(det_model)
###   mic_type: Dropdown menu for microphone type selection.
meta <- meta %>% 
  mutate(mic_type = 
           case_when(`Detector Type` == 'D500X' |
                       `Detector Type` == 'D500x' ~ 'Directional',
                     `Detector Type` == 'SM4BAT' |
                       `Detector Type` == 'SM3BAT' ~ 'SMM-U2',
                     `Detector Type` == 'Song Meter Mini Bat' |
                       `Detector Type` == 'Anabat Express' ~ 'Omni-directional',
                     `Detector Type` == 'AudioMoth' ~ 'Internal',
                     TRUE ~ 'Error'))
meta %>%
  distinct(mic_type)
###   refl_type: Dropdown menu for reflector type selection.
meta <- meta %>% 
  mutate(refl_type = 'None')

###   mic_ht: Height of the microphone above the ground (m or ft).
### BatAMP does not allow empty values, so if height not recorded, we assume it was set at the standard, 3 m
### Assume unrealistically high values (>= 5 m) are recorded as feet, convert to meters
meta %>% 
  distinct(`Microphone Height (m)`)
meta <- meta %>% 
  mutate(mic_ht = 
           case_when(is.na(`Microphone Height (m)`) ~ 3,
                     TRUE ~ `Microphone Height (m)`))
meta %>% 
  distinct(mic_ht)
###   mic_ht_units: Dropdown menu for units of measure for the microphone height.
meta <- meta %>% 
  mutate(mic_ht_units = 'meters')
###   call_id_1: Dropdown menu for primary method of call identification selection.
meta <- meta %>% 
  mutate(call_id_1 = 
           case_when(GRTS %in% c(33,289,545,1298,1569,3602) ~ 'Kaleidoscope',
                     TRUE ~ 'Sonobat 4'))
###   call_id_2: Dropdown menu for secondary method of call identification selection.
meta <- meta %>% 
  mutate(call_id_2 = 'Personal Experience')
###   site_id: Identifying name for the detector site.
meta <- meta %>% 
  mutate(site_id = paste0('CONUS_', CONUS))

meta %>% 
  distinct(site_id)
###   det_id: Identifying name for the detector location within the site (should be unique)
meta <- meta %>% 
  mutate(det_id = paste0(CONUS,'_',str_extract(`Location Name`,'[NS][EW]\\d')))
meta %>% 
  distinct(det_id)
###   night: The date of the night (M/D/YYYY). Note: by convention, night is the date recording started 
###     and includes recordings after midnight on the next calendar date.
head(meta)

### Redeployments
meta %>% 
  group_by(det_id) %>% 
  count %>% 
  filter(n>1)


meta <- meta %>% 
  mutate(SiteDeployment = 
           case_when(`Location Name` == '104648_NE1' &
                       `Deployment Date` == '2022-07-27' ~ 2,
                     `Location Name` == '104648_NW2' &
                       `Deployment Date` == '2022-07-27' ~ 2,
                     `Location Name` == '105578_NW1' &
                       `Deployment Date` == '2022-07-26' ~ 2,
                     `Location Name` == '105579_SE1' &
                       `Deployment Date` == '2022-07-25' ~ 2,
                     `Location Name` == '110673_NW2' &
                       `Deployment Date` == '2022-08-15' ~ 2,
                     `Location Name` == '111606_SW3' &
                       `Deployment Date` == '2022-06-14' ~ 2,
                     `Location Name` == '113478_NE1' &
                       `Deployment Date` == '2022-07-21' ~ 2,
                     `Location Name` == '97272_NE1' &
                       `Deployment Date` == '2022-08-22' ~ 2,
                     `Location Name` == '100552_SE1' &
                       `Deployment Date` == '2022-08-29' ~ 2, # GRTS 394 in Idaho
                     `Location Name` == '100552_SW1' &
                       `Deployment Date` == '2022-08-29' ~ 2, # GRTS 394 in Idaho
                     TRUE ~ 1)) %>% 
  select(first_name,
         last_name,
         y_coord,
         x_coord,
         det_mfg,
         det_model,
         mic_type,
         refl_type,
         mic_ht,
         mic_ht_units,
         call_id_1,
         call_id_2,
         site_id,
         det_id,
         SiteDeployment,
         `Deployment Date`)

### Bring in acoustic data
t1 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABat_AcousticData_PNW_2022.csv')
t2 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABat_AcousticData_USFWS_2022.csv')
t3 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABat_AcousticData_USFS_Region04_2022.csv')

dat <- t1 %>% 
  add_row(t2) %>% 
  add_row(t3)

dat %>% 
  distinct(`Manual Id`) %>% 
  arrange(`Manual Id`) %>% 
  print(n=Inf)


### Remove non-bat call values
dat <- dat %>% 
  filter(!(`Manual Id` %in% c(NA_character_,'40k','40kMyo','HiF','HILO','LoF','Lof','LOFRAG','MID','NoID','Noise','NOISE','Weird','x')))
dat <- dat %>% 
  filter(!str_detect(`Manual Id`, 'POS_'))
###Convert single species Code6 value to Code4
dat <- dat %>% 
  mutate(`Manual Id` =
           case_when(`Manual Id` == 'EPTFUS' ~ 'EPFU',
                     `Manual Id` == 'LASCIN' ~ 'LACI',
                     `Manual Id` == 'LASNOC' ~ 'LANO',
                     `Manual Id` == 'LOFRAG' ~ 'LOF',
                     `Manual Id` == 'MYOCAL' ~ 'MYCA',
                     `Manual Id` == 'MYOCIL' ~ 'MYCI',
                     `Manual Id` == 'MYOEVO' ~ 'MYEV',
                     `Manual Id` == 'MYOLUC' ~ 'MYLU',
                     `Manual Id` == 'MYOTHY' ~ 'MYTH',
                     `Manual Id` == 'MYOYUM' ~ 'MYYU',
                     TRUE ~ `Manual Id`))
dat <- dat %>% 
  distinct()

dat.wide <- dat %>% 
  mutate(detected = 1) %>% 
  pivot_wider(id_cols = c(GRTS,Site,SiteDeployment,MonitoringNight),
              names_from = `Manual Id`,
              values_from = detected,
              values_fn = min,
              values_fill = 0,
              names_sort = T)


### Edit Site for Idaho surveys to use CONUS_ID
dat.wide <- dat.wide %>% 
  left_join(CellList,
            by = c('GRTS' = 'GRTS_ID')) %>% 
  mutate(Site =
           case_when(GRTS == str_extract(Site,'\\d+') ~ paste0(CONUS_10KM, str_extract(Site, '_[NS][EW]\\d')),
                     TRUE ~ Site)) %>% 
  select(CONUS_10KM,GRTS,Site,SiteDeployment,MonitoringNight,ANPA:TABR)



###########################################
### Combine metadata with acoustic data ###
###########################################
head(meta)
head(dat.wide)

meta$det_id[12]
dat.wide %>% 
  filter(Site=='100990_NE2')

data <- meta %>% 
  left_join(dat.wide, by = c('det_id' = 'Site', 'SiteDeployment')) %>% 
  select(-SiteDeployment,-`Deployment Date`,-CONUS_10KM,-GRTS) %>% 
  rename('night' = 'MonitoringNight')
head(data)

### Remove surveys with no acoustic detections
data <- data %>% 
  filter(!is.na(night))



### Make night format consistent
data %>% 
  distinct(night) %>% 
  print(n=Inf)


head(data)
write_csv(data, 'C:/Users/emblidgp/Box/HERS_Working/Bats/DataRequests/BatAMP_DataRequest_2022/BatAMP_Upload_2022.csv', na = "")

### In attempting to change species values outside of range blank I noticed many detection outside of range
### Therefore we will leave all non-detections as 0, even those outside of range (which should technically be blank)
### This is consistent with previous years and the fact that during vetting any of our 15 species should be identified
### Add classifier to data
#class <- read.csv('C:/Users/emblidgp/Desktop/DataImport/NABatDataImport_C10KClassifier.csv')
#head(class)
#class <- class[,c(1,3)]

#data <- meta %>% 
#  right_join(dat.wide, by = c('det_id' = 'Site', 'SiteDeployment')) %>% 
#  left_join(class, by = c('CONUS10K' = 'CONUS_10KM')) %>% 
#  mutate(Classifier = 
#           case_when(State == 'ID' ~ 'Eastern Washington',
#                     TRUE ~ Classifier)) %>% 
#  select(-SiteDeployment,-Deployment.Date,-State,-CONUS10K,-GRTS)
#head(data)

#data %>% 
#  distinct(Classifier)

### Make values blank for species not considered, based on classifier
#spp <- read.csv('C:/Users/emblidgp/Desktop/ClassifierSpecies.csv')

#head(data)

### Western Oregon should not have MYCI, PAHE, EUMA
#MYCAMYCI, MYCI, MYLUMYCI, PAHE, EUMA
#data %>% 
#  filter(Classifier == 'Western Oregon') %>% 
#  summarize(sum(MYCAMYCI),sum(MYCI),sum(MYLUMYCI),sum(PAHE),sum(EUMA))

### Eastern Oregon can have all species

### Western Washington should not have MYCI, PAHE, ANPA, TABR, EUMA
#data %>% 
#  filter(Classifier == 'Western Washington') %>% 
#  summarize(sum(MYCAMYCI),sum(MYCI),sum(MYLUMYCI),sum(PAHE),sum(ANPA),sum(ANPAEPFU),sum(TABR),sum(TABRLACI),sum(TABRLANO),sum(EUMA))

### Eastern Washington should not have TABR
#data %>% 
#  filter(Classifier == 'Western Washington') %>% 
#  summarize(sum(TABR),sum(TABRLACI),sum(TABRLANO))