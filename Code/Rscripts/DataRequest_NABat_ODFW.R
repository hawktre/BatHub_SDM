library(tidyverse)
library(sf)
library(sfheaders)
library(readxl)
library(arcgisbinding)

### Start with metadata
meta1 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx', col_types = 'text') %>% 
  select(-c('CreationDate','Creator','EditDate','Editor','Latitude','Longitude')) %>% 
  rename('Longitude' = 'x',
         'Latitude' = 'y')
meta2 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx', col_types = 'text')
meta2 <- meta2 %>% 
  rename("Altitude" = "Altitude (m)") %>% 
  select(names(meta1))

meta <- meta1 %>% 
  add_row(meta2)

### Include all OR surveys
### Create LocationID
### Create DetectionTarget field
head(meta)
meta <- meta %>% 
  filter(`Deployment Agency` == 'ODFW') %>% 
  mutate(LocationID = paste0(`Sample Unit`, '_', Quad, `Quad Number`),
         Altitude = 
           case_when(!is.na(str_extract(Altitude, '\\d+')) ~ str_extract(Altitude, '\\d+'),
                     TRUE ~ ''),
         DetectionTarget =
           case_when(`Forest Opening Descriptor` != '' ~ paste0('ForestOpening; ', `Forest Opening Descriptor`),
                     `Forest Edge Descriptor` != '' ~ paste0('ForestEdge; ', `Forest Edge Descriptor`),
                     `Meadow Descriptor` != '' ~ paste0('Meadow; ', `Meadow Descriptor`),
                     `Rock Feature Descriptor` != '' ~ paste0('RockFeature; ', `Rock Feature Descriptor`),
                     `Dry Water Feature Descriptor` != '' ~ paste0('DryWater; ', `Dry Water Feature Descriptor`),
                     `Waterbody Descriptor` != '' ~ paste0('Waterbody; ', `Waterbody Descriptor`),
                     TRUE ~ '')) %>% 
  select(LocationID, Latitude, Longitude, Altitude, State, `Land Ownership`, `Deployment Contact(s)`, `Deployment Agency`, 
         `Habitat (choose one)`, DetectionTarget)


### Clean up Surveyor Names
meta %>% 
  distinct(`Deployment Contact(s)`) %>% 
  print(n=Inf)

meta %>% 
  filter(str_detect(`Deployment Contact(s)`,regex('krige', ignore_case = T))) %>% 
  distinct(`Deployment Contact(s)`)

meta <- meta %>% 
  mutate(Surveyor = 
           case_when(
#             `Deployment Contact(s)` == 'Brown' ~ 'E. Brown',
#                     `Deployment Contact(s)` == 'Myers' |
#                       `Deployment Contact(s)` == 'myers' ~ 'A. Myers',
#                     str_detect(`Deployment Contact(s)`, regex('rose', ignore_case = T)) ~ 'S. Rose',
#                     str_detect(`Deployment Contact(s)`, regex('asreen', ignore_case = T)) ~ 'N. Asreen',
                     str_detect(`Deployment Contact(s)`, regex('atwood', ignore_case = T)) ~ 'P. Atwood',
#                     str_detect(`Deployment Contact(s)`, regex('bolen', ignore_case = T)) ~ 'D. Bolen',
#                     str_detect(`Deployment Contact(s)`, regex('brecka', ignore_case = T)) ~ 'B. Brecka',
#                     str_detect(`Deployment Contact(s)`, regex('brewster', ignore_case = T)) ~ 'L. Brewster',
#                     str_detect(`Deployment Contact(s)`, regex('calderon', ignore_case = T)) ~ 'F. Calderon',
#                     str_detect(`Deployment Contact(s)`, regex('cate', ignore_case = T)) ~ 'B. Cate',
                     str_detect(`Deployment Contact(s)`, regex('cannaday', ignore_case = T)) ~ 'B. Cannaday',
                     str_detect(`Deployment Contact(s)`, regex('chamberlain', ignore_case = T)) ~ 'C. Chamberlain',
#                     str_detect(`Deployment Contact(s)`, regex('cherry', ignore_case = T)) ~ 'S. Cherry',
                     str_detect(`Deployment Contact(s)`, regex('collom', ignore_case = T)) ~ 'T. Collom',
#                     str_detect(`Deployment Contact(s)`, regex('dalton', ignore_case = T)) ~ 'J. Dalton',
                     str_detect(`Deployment Contact(s)`, regex('darr', ignore_case = T)) |
  str_detect(`Deployment Contact(s)`, regex('krige', ignore_case = T)) ~ 'A. Darr',
                     str_detect(`Deployment Contact(s)`, regex('foster', ignore_case = T)) ~ 'L. Foster',
                     str_detect(`Deployment Contact(s)`, regex('gerrity', ignore_case = T)) ~ 'S. Gerrity',
#                     str_detect(`Deployment Contact(s)`, regex('gutcher', ignore_case = T)) ~ 'J. Gutcher',
                     str_detect(`Deployment Contact(s)`, regex('hardt', ignore_case = T)) ~ 'L. Hardt',
#                     str_detect(`Deployment Contact(s)`, regex('heinlen', ignore_case = T)) ~ 'J. Heinlen',
#                     str_detect(`Deployment Contact(s)`, regex('kelly', ignore_case = T)) ~ 'P. Kelly',
#                     str_detect(`Deployment Contact(s)`, regex('kirchner', ignore_case = T)) ~ 'J. Kirchner',
#                     str_detect(`Deployment Contact(s)`, regex('klos', ignore_case = T)) ~ 'R. Klus',
#                     str_detect(`Deployment Contact(s)`, regex('klus', ignore_case = T)) ~ 'R. Klus',
#                     str_detect(`Deployment Contact(s)`, regex('laeske', ignore_case = T)) ~ 'C. Laeske',
#                     str_detect(`Deployment Contact(s)`, regex('landsiedel', ignore_case = T)) ~ 'H. Landsiedel',
#                     str_detect(`Deployment Contact(s)`, regex('langley', ignore_case = T)) ~ 'S. Langley',
                     str_detect(`Deployment Contact(s)`, regex('lewis', ignore_case = T)) ~ 'R. Lewis',
#                     str_detect(`Deployment Contact(s)`, regex('licence', ignore_case = T)) ~ 'K. Licence',
#                     str_detect(`Deployment Contact(s)`, regex('lowe', ignore_case = T)) ~ 'J. Lowe',
#                     str_detect(`Deployment Contact(s)`, regex('luther', ignore_case = T)) ~ 'L. Luther',
#                     str_detect(`Deployment Contact(s)`, regex('mccarroll', ignore_case = T)) ~ 'B. McCarroll',
#                     str_detect(`Deployment Contact(s)`, regex('melnick', ignore_case = T)) ~ 'D. Melnick',
                     str_detect(`Deployment Contact(s)`, regex('meyers', ignore_case = T)) ~ 'A. Meyers',
                     str_detect(`Deployment Contact(s)`, regex('myers', ignore_case = T)) ~ 'A. Myers',
#                     str_detect(`Deployment Contact(s)`, regex('moore', ignore_case = T)) ~ 'M. Moore',
#                     str_detect(`Deployment Contact(s)`, regex('mcnassar', ignore_case = T)) ~ 'G. McNassar',
                     str_detect(`Deployment Contact(s)`, regex('muir', ignore_case = T)) ~ 'J. Muir',
                     str_detect(`Deployment Contact(s)`, regex('nuzum', ignore_case = T)) ~ 'D. Nuzum',
                     str_detect(`Deployment Contact(s)`, regex('ott', ignore_case = T)) ~ 'T. Ott',
                     str_detect(`Deployment Contact(s)`, regex('pifer', ignore_case = T)) ~ 'J. Pifer',
#                     str_detect(`Deployment Contact(s)`, regex('ratliff', ignore_case = T)) ~ 'J. Ratliff',
#                     str_detect(`Deployment Contact(s)`, regex('schenk', ignore_case = T)) ~ 'A. Schenk',
#                     str_detect(`Deployment Contact(s)`, regex('schmidt', ignore_case = T)) ~ 'N. Schmidt',
#                     str_detect(`Deployment Contact(s)`, regex('scoggins', ignore_case = T)) ~ 'M. Scoggins',
#                     str_detect(`Deployment Contact(s)`, regex('smith', ignore_case = T)) ~ 'L. Smith',
                     str_detect(`Deployment Contact(s)`, regex('stack', ignore_case = T)) ~ 'J. Stack',
#                     str_detect(`Deployment Contact(s)`, regex('templeman', ignore_case = T)) ~ 'L. Templeman',
#                     str_detect(`Deployment Contact(s)`, regex('r. torland', ignore_case = T)) ~ 'R. Torland',
#                     str_detect(`Deployment Contact(s)`, regex('s. torland', ignore_case = T)) ~ 'S. Torland',
#                     str_detect(`Deployment Contact(s)`, regex('vaughn', ignore_case = T)) ~ 'J. Vaughn',
                     str_detect(`Deployment Contact(s)`, regex('walch', ignore_case = T)) ~ 'A. Walch',
#                     str_detect(`Deployment Contact(s)`, regex('williams', ignore_case = T)) ~ 'B. Williams',
#                     str_detect(`Deployment Contact(s)`, regex('wyatt', ignore_case = T)) ~ 'M. Wyatt',
#                     str_detect(`Deployment Contact(s)`, regex('yates', ignore_case = T)) ~ 'K. Yates',
#                     `Deployment Contact(s)` == 'Pelikan' ~ 'Pelikan',
                     `Deployment Contact(s)` == 'Jade Keehn' ~ 'J. Keehn',
#                     `Deployment Contact(s)` == 'HThompson' ~ 'H. Thompson',
                     TRUE ~ `Deployment Contact(s)`))
meta %>% 
  distinct(Surveyor) %>% 
  print(n=Inf)


### Clean up habitat
meta <- meta %>% 
  mutate('Habitat' =
           case_when(`Habitat (choose one)` == 'dryconifer' ~ 'dry conifer',
                     `Habitat (choose one)` == 'mixedconifer' ~ 'mixed conifer',
                     `Habitat (choose one)` == 'mesicforest' ~ 'mesic forest',
                     `Habitat (choose one)` == 'alpineforest' ~ 'alpine forest',
                     `Habitat (choose one)` == 'shrub-stepp' ~ 'shrub-steppe',
                     `Habitat (choose one)` == 'agriculture; grassland' ~ 'agriculture',
                     `Habitat (choose one)` == 'urban; shrub-steppe' ~ 'urban',
                     `Habitat (choose one)` == 'dry conifer; agriculture' ~ 'dry conifer',
                     TRUE ~ `Habitat (choose one)`))
meta %>% 
  distinct(`Habitat (choose one)`)
meta %>% 
  distinct(Habitat)


### Format metadata
meta <- meta %>% 
  select(LocationID,Latitude,Longitude,Altitude,State,`Land Ownership`,Surveyor,`Deployment Agency`,Habitat,DetectionTarget) %>% 
  rename('Ownership' = 'Land Ownership',
         'Elevation' = 'Altitude',
         'Organization' = 'Deployment Agency')


#######################################
### Revisits are a bit of a problem ###
#######################################
### I could join metadata to acoustic data by date, but I'm assuming that the metadata should be similar
###   between revisits, so just remove duplicate site visits
### Look for revisits
meta %>% 
  group_by(LocationID) %>% 
  count %>% 
  filter(n>1)

### Make sure metadata is similar between revisits
meta %>% 
  filter(LocationID == '113478_NE1')

### Remove records with duplicated LocationID to earlier rows
meta <- meta %>% 
  filter(!duplicated(LocationID))


### Remove metadata with missing coordinates
meta %>% 
  filter(Latitude==0)
meta %>% 
  filter(Latitude=='')
meta %>% 
  filter(is.na(Latitude))
### 105615_SE1 No Coordinates, not surveyed
### 96350_SE2 No Coordinates, no acoustic files
meta <- meta %>% 
  filter(!(LocationID %in% c('96350_Unk1','96350_Unk2','96350_Unk3')))



### Convert metadata to Simple Feature
meta.sf <- st_as_sf(meta, coords = c('Longitude','Latitude'))
st_crs(meta.sf) <- 4269
meta.sf


### Read in OR counties, join to metadata
# Finalize the R-ArcGIS bridge
arc.check_product()

# Read in USA Census Counties
counties <- arc.open('https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_Counties/FeatureServer/0')
counties <- arc.select(counties)
counties <- arc.data2sf(counties)
counties <- st_make_valid(counties)
counties <- st_transform(counties,4269)

#ORcounties <- st_read('C:/Users/emblidgp/Downloads/countiesOR/county_nrcs_a_or.shp')
#ORcounties <- st_transform(ORcounties, 4269)

sf_use_s2(FALSE)
meta.sf <- meta.sf %>% 
  st_join(counties, join = st_intersects)
sf_use_s2(TRUE)

meta.sf %>% 
  distinct(NAME) %>% 
  print(n=Inf)

meta.sf %>% 
  filter(is.na(NAME)) %>% 
  distinct(LocationID)

meta.sf <- meta.sf %>% 
  mutate(County = str_remove(NAME," County")) %>% 
  select(LocationID,Elevation,State,Ownership,Surveyor,Organization,Habitat,DetectionTarget,County)


### Remove the two surveys occurring in NV
meta.sf <- meta.sf %>% 
  filter(!is.na(County))


### Convert simple feature to data.frame
### Clean metadata
meta <- meta.sf %>% 
  sf_to_df(fill=T) %>% 
  mutate(Year = '2022',
         Datum = 'NAD83') %>% 
  select(LocationID,Year,Datum,y,x,Elevation,State,County,Ownership,Surveyor,Organization,Habitat,DetectionTarget) %>% 
  rename('Latitude' = 'y',
         'Longitude' = 'x')


### Read in all acoustic data
dat <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABat_AcousticData_PNW_2022.csv')
head(dat)
dim(dat)

dat <- dat %>% 
  left_join(meta, by = c('Site' = 'LocationID')) %>% 
  filter(!is.na(County)) %>% 
  select(Site,MonitoringNight,Datum,Latitude,Longitude,Elevation,State,County,Ownership,Surveyor,Organization,Habitat,DetectionTarget,`Manual Id`) %>% 
  rename('LocationID' = 'Site',
         'Date_Obs' = 'MonitoringNight')

dim(dat)
head(dat)


bats <- read.csv('C:/Users/emblidgp/Desktop/BatList.csv')

dat <- dat %>% 
  left_join(bats, by = c('Manual Id' = 'Code')) %>% 
  filter(!is.na(SpeciesName)) %>% 
  distinct() %>% 
  select(-`Manual Id`)


### Check that there's not duplicate records, usually caused by revisit metadata
dim(dat)
dat %>% 
  distinct(LocationID,Date_Obs,SpeciesName) %>% 
  dim()

### Look at surveys longer than 1 night
dat %>% 
  group_by(LocationID, SpeciesName) %>% 
  count %>% 
  filter(n>1)
dat %>% 
  filter(LocationID == '102349_NW1' &
           SpeciesName == 'Eptesicus fuscus ')

### Remove duplicate Site/Species detections during season
### This is not what Roger wanted
#dat1 <- dat1 %>% 
#  mutate(tmp = paste0(LocationID,'_',SpeciesName)) %>% 
#  arrange(LocationID,SpeciesName,Date_Obs) %>% 
#  filter(!duplicated(tmp)) %>% 
#  select(-tmp)

dat <- dat %>% 
  arrange(LocationID,SpeciesName,Date_Obs)

View(dat)
#write_csv(dat, 'C:/Users/emblidgp/Box/HERS_Working/Bats/DataRequests/ODFW_DataRequest_2022/DataRequest_NABat_ODFW_2022.csv')
write_csv(dat, 'C:/Users/emblidgp/Box/HERS_Working/Bats/DataRequests/IDFG_DataRequest_2022/DataRequest_NABat_IDFG_2022.csv')


##########################################
### Combined LACI and MYLU 2020 - 2021 ###
##########################################
dat20 <- read.csv('C:/Users/emblidgp/Desktop/DataRequests/ODFW/ODFW_NABat Data_Request_2020.csv')
dat20$Date_Obs <- mdy(dat20$Date_Obs)
head(dat20)
dat21 <- read.csv('C:/Users/emblidgp/Desktop/DataRequests/ODFW/DataRequest_NABat_ODFW_2021.csv')
head(dat21)
names(dat20) <- names(dat21)


dat <- rbind(dat20,dat21)
dat <- dat %>% 
  filter(SpeciesName %in% c('Lasiurus cinereus ', 'Myotis lucifugus ')) %>% 
  arrange(LocationID,SpeciesName,Date_Obs)

dat <- dat %>% 
  mutate(SpeciesName = 
           case_when(SpeciesName == 'Lasiurus cinereus ' ~ 'Lasiurus cinereus',
                     SpeciesName == 'Myotis lucifugus ' ~ 'Myotis lucifugus'))

dat %>% 
  distinct(SpeciesName)

#write.csv(dat, 'C:/Users/emblidgp/Desktop/DataRequests/ODFW/DataRequest_NABat_ODFW_LACI_MYLU_2020_2021.csv', row.names = F)
