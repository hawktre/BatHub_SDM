library(tidyverse)
library(readxl)
library(sf)
library(arcgisbinding)
library(sfheaders)

### Start with metadata
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


# Read in PAD-US 3.0 Fee Manager
arc.check_product()
admin <- arc.open("https://services.arcgis.com/v01gqwM5QqNysAAi/arcgis/rest/services/Fee_Manager/FeatureServer/0")
select_statement <- "State_Nm IN ('OR','WA') AND Loc_Mang = 'BLM'"
admin <- arc.select(admin,where_clause = select_statement)
admin <- arc.data2sf(admin)
admin <- st_make_valid(admin)
admin <- st_transform(admin,4326)


# Read in USA Counties
counties <- arc.open("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_Counties/FeatureServer/0")
select_statement <- "STATE_NAME IN ('Oregon','Washington')"
counties <- arc.select(counties, where_clause = select_statement)
counties <- arc.data2sf(counties)
counties <- st_make_valid(counties)
counties <- st_transform(counties,4326)



### Include only OR and WA surveys on BLM managed lands
### Create LocationID
### Create DetectionTarget field
meta.sf <- meta.sf %>% 
  st_join(admin, join = st_intersects) %>% 
  st_join(counties, join = st_intersects) %>% 
  filter(!is.na(Loc_Mang)) %>% 
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

# Add Site Deployment
meta.sf %>% 
  group_by(StationLocation) %>% 
  count() %>% 
  filter(n>1)

meta.sf <- meta.sf %>% 
  mutate(SiteDeployment =
           case_when(StationLocation == '105578_NW1' &
                       `Deployment Date` == '2022-07-26' ~ 2,
                     StationLocation == '105579_SE1' &
                       `Deployment Date` == '2022-07-25' ~ 2,
                     StationLocation == '113478_NE1' &
                       `Deployment Date` == '2022-07-21' ~ 2,
                     StationLocation == '97272_NE1' &
                       `Deployment Date` == '2022-08-22' ~ 2,
                     TRUE ~ 1))

meta.sf %>% 
  filter(SiteDeployment ==2)


### Clean up Surveyor Names
meta.sf %>% 
  distinct(`Deployment Contact(s)`,`Deployment Agency`) %>% 
  print(n=Inf)
meta.sf <- meta.sf %>% 
  mutate(Surveyor = 
           case_when(`Deployment Contact(s)` == "Brown" |
                       str_detect(`Deployment Contact(s)`, regex('myers', ignore_case = T)) &
                       `Deployment Agency` == "OSU" ~ "E. Brown",
                     `Deployment Contact(s)` == 'Tom Collom' ~ 'T. Collom',
                     `Deployment Contact(s)` == 'Jade Keehn' ~ 'J. Keehn',
                     str_detect(`Deployment Contact(s)`, regex('foster', ignore_case = T)) ~ 'L. Foster',
                     str_detect(`Deployment Contact(s)`, regex('hardt', ignore_case = T)) ~ 'A. Walch',
                     str_detect(`Deployment Contact(s)`, regex('cannaday', ignore_case = T)) ~ 'T. Lum',
                     str_detect(`Deployment Contact(s)`, regex('marianne brooks', ignore_case = T)) ~ 'C. Yee',
                     str_detect(`Deployment Contact(s)`, regex('scoggins', ignore_case = T)) ~ 'M. Scoggins',
                     str_detect(`Deployment Contact(s)`, regex('ott', ignore_case = T)) ~ 'A. Meyers',
                     str_detect(`Deployment Contact(s)`, regex('macmillan', ignore_case = T)) ~ 'J. Macmillan',
                     str_detect(`Deployment Contact(s)`, regex('schwartz', ignore_case = T)) ~ 'M. Schwartz',
                     str_detect(`Deployment Contact(s)`, regex('hodgkins', ignore_case = T)) ~ 'J. Hodgkins',
                     `Deployment Contact(s)` == 'Jon Muir' ~ 'J. Muir',
                     `Deployment Contact(s)` == 'Lee Templeman' ~ 'L. Templeman',
                     `Deployment Contact(s)` == 'Jeff Heinlen' ~ 'J. Heinlen',
                     TRUE ~ `Deployment Contact(s)`))
meta.sf %>% 
  distinct(Surveyor) %>% 
  print(n=Inf)


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
  select(StationLocation,SiteDeployment,`Deployment Date`,Datum,x,y,Altitude,STATE_ABBR,NAME,`Land Ownership`,Surveyor,`Deployment Agency`,Habitat,DetectionTarget) %>% 
  rename('Longitude' = 'x',
         'Latitude' = 'y',
         'State' = 'STATE_ABBR',
         'County' = 'NAME',
         'Ownership' = 'Land Ownership',
         'Organization' = 'Deployment Agency',
         'Elevation' = 'Altitude')


### Read in species richness data
SpRich <- read_csv("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/SppRichness_PNW_2022.csv")


data <- meta %>% 
  left_join(SpRich, by = c('StationLocation' = 'Site',
                           'SiteDeployment'))

data2 <- data %>% 
  pivot_longer(ANPA:TABR, names_to = 'SpeciesCode') %>% 
  filter(value == 1)

bats <- read.csv('C:/Users/emblidgp/Desktop/BatList.csv')


data2 <- data2 %>% 
  left_join(bats, by = c('SpeciesCode' = 'Code')) %>% 
  distinct() %>% 
  select(-c(SiteDeployment,SpeciesCode,UnusualOccurrences,value,GRTS,Ownership))


head(data2)
write_csv(data2, 'C:/Users/emblidgp/Box/HERS_Working/Bats/DataRequests/BLM_DataRequest_2022/DataRequest_NABat_BLM_2022.csv')






### Starting from scratch because above output had some gaps
### In ArcGIS Pro, using NABatSurveys2021_Completed and USA Protected Sites - Fee Managers layers,
### I selected all surveys on BLM land in OR (159) and exported as Table
library(tidyverse)
library(readxl)
library(readr)

rich <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/2021_Analysis/SppRichness2021.xlsx', sheet = 'SppRichness2021')
sites <- read.csv('C:/Users/emblidgp/Desktop/NABatSurveys2021_Completed_BLM_OR.csv')
sites <- read.csv('C:/Users/emblidgp/Desktop/NABatSurveys2021_Completed_BLM_WA.csv')
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx', col_types = 'text') %>% 
  select(1:2)

dim(sites)
sites %>% 
  group_by(LocationName) %>% 
  count() %>% 
  filter(n>1)


### A couple duplicate sites, but they are already accounted for in richness table
sites %>% 
  filter(LocationName=='107020_NW1')
sites %>% 
  filter(LocationName=='97272_NE2')
rich %>% 
  filter(Site=='107020_NW1')
rich %>% 
  filter(Site=='97272_NE2')


sites <- sites %>% 
  mutate(CONUS10K = str_extract(LocationName,'\\d+')) %>% 
  left_join(CellList, by = c('CONUS10K' = 'CONUS_10KM')) %>% 
  rename(GRTS = GRTS_ID,
         Site = LocationName)


### Raw acoustic files
# OR = 126,489
# WA = 13,032
# Total = 139,521
sum(sites$filesRecorded)


rich <- rich %>% 
  filter(Site %in% sites$Site) %>% 
  mutate(Note = NA_character_)

### Are there BLM sites without records in rich?
sites %>% 
  filter(!(Site %in% rich$Site))

sites <- sites %>% 
  filter(!(Site %in% rich$Site)) %>% 
  mutate(Note =
           case_when(filesRecorded == 0 ~ 'Detector Failure',
                     TRUE ~ NA_character_),
         ANPA = 0,
         COTO = 0,
         EPFU = 0,
         EUMA = 0,
         LACI = 0,
         LANO = 0,
         MYCA = 0,
         MYCI = 0,
         MYEV = 0,
         MYLU = 0,
         MYTH = 0,
         MYVO = 0,
         MYYU = 0,
         PAHE = 0,
         TABR = 0) %>% 
  select(2,12,13,3,15:29,14)

rich$CONUS10K <- as.character(rich$CONUS10K)
rich$GRTS <- as.character(rich$GRTS)

rich <- rich %>% 
  add_row(sites) %>% 
  arrange(Site)

#write.csv(rich,'C:/Users/emblidgp/Desktop/NABatSurveys2021_Completed_BLM_OR_Richness.csv',row.names = F)
#write.csv(rich,'C:/Users/emblidgp/Desktop/NABatSurveys2021_Completed_BLM_WA_Richness.csv',row.names = F)

rich %>% 
  distinct(CONUS10K)

meta <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/2021_Analysis/NABatMetadata2021.xlsx', sheet = 'NABatMetadata')


### Duplicate records for Stations on BLM land?
meta %>% 
  mutate(Station = paste0(`Sample Unit`,'_',Quad,`Quad Number`)) %>% 
  filter(Station %in% rich$Site) %>% 
  group_by(Station) %>% 
  count() %>% 
  filter(n>1)

meta %>% 
  mutate(Station = paste0(`Sample Unit`,'_',Quad,`Quad Number`)) %>% 
  filter(Station == '107020_NW1') %>% 
  View()
meta %>% 
  mutate(Station = paste0(`Sample Unit`,'_',Quad,`Quad Number`)) %>% 
  filter(Station == '97272_NE2') %>% 
  View()

### Remove
# 107020_NW1 2021-06-23
# 107020_NW1 2021-07-07
# 97272_NE2 2021-06-14

meta <- meta %>% 
  mutate(Station = paste0(`Sample Unit`,'_',Quad,`Quad Number`),
         `Deployment Date` = as.Date(`Deployment Date`)) %>% 
  filter(Station %in% rich$Site &
           (!(Station == '107020_NW1' &
                `Deployment Date` == '2021-06-23')) &
           (!(Station == '107020_NW1' &
                `Deployment Date` == '2021-07-07')) &
           (!(Station == '97272_NE2' &
                `Deployment Date` == '2021-06-14')))


rich <- rich %>% 
  left_join(meta, 
            by = c('Site' = 'Station'))


rich <- rich %>% 
  mutate(Latitude =
           case_when(is.na(Latitude) ~ y,
                     TRUE ~ Latitude),
         Longitude = 
           case_when(is.na(Longitude) ~ x,
                     TRUE ~ Longitude),
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
  select(Site, `Deployment Date`, Latitude, Longitude, Altitude, State.x, `Deployment Contact(s)`, Creator, `Deployment Agency`, 
         `Habitat (choose one)`, DetectionTarget,ANPA,COTO,EPFU,EUMA,LACI,LANO,MYCA,MYCI,MYEV,MYLU,MYTH,MYVO,
         MYYU,PAHE,TABR) %>% 
  rename(State = State.x)


### Clean up Surveyor Names
rich %>% 
  distinct(`Deployment Contact(s)`)

rich <- rich %>% 
  mutate(Surveyor = 
           case_when(str_detect(`Deployment Contact(s)`, regex('rose', ignore_case = T)) ~ 'S. Rose',
                     str_detect(`Deployment Contact(s)`, regex('klus', ignore_case = T)) ~ 'R. Klus',
                     str_detect(`Deployment Contact(s)`, regex('klos', ignore_case = T)) ~ 'R. Klus',
                     `Deployment Contact(s)` == 'Pelikan' ~ 'R. Klus',
                     str_detect(`Deployment Contact(s)`, regex('Landsiedel', ignore_case = T)) ~ 'H. Landsiedel',
                     str_detect(`Deployment Contact(s)`, regex('yates', ignore_case = T)) ~ 'K. Yates',
                     str_detect(`Deployment Contact(s)`, regex('moore', ignore_case = T)) ~ 'M. Moore',
                     str_detect(`Deployment Contact(s)`, regex('torland', ignore_case = T)) ~ 'S. Torland',
                     str_detect(`Deployment Contact(s)`, regex('brecka', ignore_case = T)) ~ 'B. Brecka',
                     str_detect(`Deployment Contact(s)`, regex('gutcher', ignore_case = T)) ~ 'J. Gutcher',
                     `Deployment Contact(s)` == 'Jade Keehn' ~ 'J. Keehn',
                     `Deployment Contact(s)` == 'LSmith' ~ 'L. Smith',
                     `Deployment Contact(s)` == 'HThompson' ~ 'H. Thompson',
                     str_detect(`Deployment Contact(s)`, regex('luther', ignore_case = T)) ~ 'L. Luther',
                     str_detect(`Deployment Contact(s)`, regex('schenk', ignore_case = T)) ~ 'A. Schenk',
                     str_detect(`Deployment Contact(s)`, regex('laeske', ignore_case = T)) ~ 'C. Laeske',
                     str_detect(`Deployment Contact(s)`, regex('langley', ignore_case = T)) ~ 'Langley',
                     str_detect(`Deployment Contact(s)`, regex('cannaday', ignore_case = T)) ~ 'B. Cannaday',
                     str_detect(`Deployment Contact(s)`, regex('melnick', ignore_case = T)) ~ 'D. Melnick',
                     str_detect(`Deployment Contact(s)`, regex('lewis', ignore_case = T)) ~ 'R. Lewis',
                     str_detect(`Deployment Contact(s)`, regex('stack', ignore_case = T)) ~ 'J. Stack',
                     str_detect(`Deployment Contact(s)`, regex('meyers', ignore_case = T)) ~ 'A. Meyers',
                     str_detect(`Deployment Contact(s)`, regex('bolen', ignore_case = T)) ~ 'D. Bolen',
                     str_detect(`Deployment Contact(s)`, regex('flor', ignore_case = T)) ~ 'F. Calderon',
                     str_detect(`Deployment Contact(s)`, regex('lowe', ignore_case = T)) ~ 'J. Lowe',
                     str_detect(`Deployment Contact(s)`, regex('heinlen', ignore_case = T)) ~ 'J. Heinlen',
                     str_detect(`Deployment Contact(s)`, regex('cherry', ignore_case = T)) ~ 'S. Cherry',
                     str_detect(`Deployment Contact(s)`, regex('templeman', ignore_case = T)) ~ 'L. Templeman',
                     str_detect(`Deployment Contact(s)`, regex('scoggins', ignore_case = T)) ~ 'M. Scoggins',
                     TRUE ~ ''))

### Clean up habitat
rich %>% 
  distinct(`Habitat (choose one)`)
rich <- rich %>% 
  mutate('Habitat' =
           case_when(`Habitat (choose one)` == 'dryconifer' ~ 'dry conifer',
                     `Habitat (choose one)` == 'mixedconifer' ~ 'mixed conifer',
                     `Habitat (choose one)` == 'mesicforest' ~ 'mesic forest',
                     `Habitat (choose one)` == 'shrub-stepp' ~ 'shrub-steppe',
                     TRUE ~ `Habitat (choose one)`))

### Format metadata
rich <- rich %>% 
  select(Site,`Deployment Date`,Latitude,Longitude,Altitude,State,Surveyor,`Deployment Agency`,Habitat,DetectionTarget,
         ANPA,COTO,EPFU,EUMA,LACI,LANO,MYCA,MYCI,MYEV,MYLU,MYTH,MYVO,MYYU,PAHE,TABR) %>% 
  rename('Elevation' = 'Altitude')


### Convert metadata to Simple Feature
rich.sf <- st_as_sf(rich, coords = c('Longitude','Latitude'))
st_crs(rich.sf) <- 4269
rich.sf

### Read in WA counties, join to metadata
WAcounties <- st_read('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/WA NABat/WA_Counties/WA_County_Boundaries.shp')
WAcounties <- st_transform(WAcounties, 4269)

rich.sf <- rich.sf %>% 
  st_join(WAcounties)

rich.sf <- rich.sf %>% mutate(County = JURISDIC_2) %>% 
  select(Site,`Deployment Date`,Elevation,State,Surveyor,`Deployment Agency`,Habitat,DetectionTarget,County,
         ANPA,COTO,EPFU,EUMA,LACI,LANO,MYCA,MYCI,MYEV,MYLU,MYTH,MYVO,MYYU,PAHE,TABR)


### Read in OR counties, join to metadata
ORcounties <- st_read('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/OR_Counties/orcntypoly.shp')
ORcounties <- st_transform(ORcounties, 4269)

rich.sf <- rich.sf %>% 
  st_join(ORcounties)


rich.sf <- rich.sf %>% 
  mutate(County = altName) %>% 
  select(Site,`Deployment Date`,Elevation,State,Surveyor,`Deployment Agency`,Habitat,DetectionTarget,County,
         ANPA,COTO,EPFU,EUMA,LACI,LANO,MYCA,MYCI,MYEV,MYLU,MYTH,MYVO,MYYU,PAHE,TABR)


### Convert simple feature to data.frame
library(sfheaders)

### Clean metadata
rich <- rich.sf %>% 
  sf_to_df(fill=T) %>% 
  mutate(Year = '2021',
         Datum = 'NAD83') %>% 
  select(Site,`Deployment Date`,Datum,y,x,Elevation,State,County,Surveyor,`Deployment Agency`,Habitat,DetectionTarget,
         ANPA,COTO,EPFU,EUMA,LACI,LANO,MYCA,MYCI,MYEV,MYLU,MYTH,MYVO,MYYU,PAHE,TABR) %>% 
  rename('Latitude' = 'y',
         'Longitude' = 'x')


#write.csv(rich, 'C:/Users/emblidgp/Desktop/DataRequest_NABat_BLM_2021_OR_SppRichness_Metadata.csv', row.names = F)
#write.csv(rich, 'C:/Users/emblidgp/Desktop/DataRequest_NABat_BLM_2021_WA_SppRichness_Metadata.csv', row.names = F)

