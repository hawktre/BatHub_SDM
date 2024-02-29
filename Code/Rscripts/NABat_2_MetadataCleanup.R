### This script is used to prepare metadata for upload to NABat
### Prior to running this script, you will want to disable editing in Field Maps, 
#### export NABat_Survey_Form_{Year] CSV from AGOL,
#### create a new Excel Spreadsheet on Box, Box\HERS_Working\Bats\Analysis_NABat\{Year}_Analysis\NABatMetadata{Year}_FieldMaps.xlsx,
#### copy the NABat_Survey_Form_{Year] CSV into this new empty spreadsheet, excluding ObjectID and GlobalID fields, and
#### add new field 'UnusualOccurrences' and populate from notes in 'Comments' field

# Install and load packages ----
if(!'pacman' %in% installed.packages()){
  install.packages('pacman')
}

pacman::p_load(tidyverse,
               readxl,
               lubridate,
               lutz,
               DBI,
               chron,
               bioRad,
               sf)

# Identify yourself and year of data to upload ----
onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")

# Prepare metadata for NABat upload ----
## Read in and format full metadata file(s) ----
### FieldMaps ----
meta1 <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_FieldMaps.xlsx')) %>% 
  mutate(LocationName = paste0(CONUS,'_',Quad,`Quad Number`),
         LocationName_GRTS = paste0(GRTS,'_',Quad,`Quad Number`))
names(meta1)

# x and y are complete coordinate columns
# In AGOL times are recorded in local time and stored as UTC, convert to local time of site and print as character.
# r does not tolerate multiple time zones in same column

# Identify the time zone of each survey based on coordinates
meta1 <- meta1 %>% 
  mutate(zone = tz_lookup_coords(y,x,method = 'accurate'))

# What time zones did we survey in
meta1 %>% 
  distinct(zone)

# Create Survey Start and End Time as character string in local time zone
# In the future, double check OSU, OR crew CONUS 99605 and 99612, Sample Units are in MDT zone, but tablet may not have updated from PDT. 
meta1 <- meta1 %>% 
  mutate('Survey Start Time' =
           case_when(zone == 'America/Boise' ~
                       as.character(with_tz(`Recording Start Time`, tzone = 'America/Boise')),
                     zone == 'America/Los_Angeles' ~
                       as.character(with_tz(`Recording Start Time`, tzone = 'America/Los_Angeles'))),
         'Survey End Time' =
           case_when(zone == 'America/Boise' ~
                       as.character(with_tz(`Recording Stop Time`, tzone = 'America/Boise')),
                     zone == 'America/Los_Angeles' ~
                       as.character(with_tz(`Recording Stop Time`, tzone = 'America/Los_Angeles'))))

# Check Survey Start/End Times, general check to make sure format looks right
meta1 %>% 
  select(`Deployment Date`,`Recovery Date`,`Recording Start Time`,`Recording Stop Time`,`Survey Start Time`,`Survey End Time`) %>% 
  arrange(`Survey Start Time`) %>% 
  print(n=Inf)

# Check Survey Start/End Times for long deployments (> 4 days)
meta1 %>% 
  select(LocationName,LocationName_GRTS,`Deployment Contact(s)`,`Deployment Date`,`Recovery Date`,`Survey Start Time`,`Survey End Time`) %>% 
  mutate(duration = difftime(ymd_hms(`Survey End Time`),ymd_hms(`Survey Start Time`),units = 'days')) %>% 
  filter(duration > 4) %>% 
  arrange(desc(duration)) %>% 
  print(n=Inf)

# Check Survey Start/End Times for negative length deployments
meta1 %>% 
  select(LocationName,LocationName_GRTS,`Deployment Contact(s)`,`Deployment Date`,`Recovery Date`,`Survey Start Time`,`Survey End Time`) %>% 
  mutate(duration = difftime(ymd_hms(`Survey End Time`),ymd_hms(`Survey Start Time`),units = 'days')) %>% 
  filter(duration < 0) %>% 
  arrange(desc(duration)) %>% 
  print(n=Inf)

meta1 <- meta1 %>% 
  select(GRTS,
         LocationName,
         LocationName_GRTS,
         `New Site?`,
         x,
         y,
         `Survey Start Time`,
         `Survey End Time`,
         `Detector Type`,
         `Detector Serial No.`,
         `Microphone Orientation`,
         `Microphone Height (m)`,
         `Distance to Clutter (m)`,
         `Clutter Type`,
         `Waterbody Descriptor`,
         `Clutter Category`,
         `Habitat (choose one)`,
         `Deployment Contact(s)`,
         `Deployment Agency`,
         `Enter other Agency`,
         UnusualOccurrences) %>% 
  rename('Latitude' = 'y',
         'Longitude' = 'x')

### Paper Datasheets ----
meta2 <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_PaperDatasheets.xlsx')) %>% 
  mutate(LocationName = paste0(CONUS,'_',Quad,`Quad Number`),
         LocationName_GRTS = paste0(GRTS,'_',Quad,`Quad Number`))
names(meta2)

### Create Survey Start and End Time as character string in local time zone, filling in blanks with default values. 
meta2 <- meta2 %>% 
  mutate(`Deployment Date` = as.character(`Deployment Date`),
         `Recovery Date` = as.character(`Recovery Date`),
         `Survey Start Time` = NA_character_,
         `Survey End Time` = NA_character_)

meta2 %>% 
  distinct(`Deployment Date`) %>% 
  arrange(`Deployment Date`) %>% 
  print(n=Inf)

### Go back into metadata and make Deployment Date and Recovery Date corrections if needed
meta2 %>% 
  filter(is.na(`Deployment Date`) |
           is.na(`Recovery Date`))

# Calculate Survey Start/End times
for(i in 1:length(meta2$State)){
  if(is.na(meta2$`Recording Start Time`[i])){
    meta2$`Survey Start Time`[i] <- paste0(meta2$`Deployment Date`[i],' 19:30:00')
  }else{
    if(meta2$`Recording Start Time`[i] < 1 &
       meta2$`Recording Start Time`[i] > 0){
      meta2$`Survey Start Time`[i] <- paste(meta2$`Deployment Date`[i],times(as.numeric(meta2$`Recording Start Time`[i])))
  }else{
    if(str_detect(meta2$`Recording Start Time`[i],"-\\d{2}:\\d{2}")){
      tmp1 <- tz_lookup_coords(meta2$Latitude[i],meta2$Longitude[i],method = 'accurate')
      meta2$`Survey Start Time`[i] <- 
        format(sunset(as_datetime(meta2$`Deployment Date`[i],tz=tmp1),meta2$Longitude[i],meta2$Latitude[i],tz=tmp1) - 
                 lubridate::hours(as.numeric(str_extract(meta2$`Recording Start Time`[i],"(?<=-)\\d{2}(?=:)"))) - 
                 lubridate::minutes(as.numeric(str_extract(meta2$`Recording Start Time`[i],"(?<=:)\\d{2}$"))),"%Y-%m-%d %H:%M:%S")
    }else{NA}}}}

for(i in 1:length(meta2$State)){
  if(is.na(meta2$`Recording Stop Time`[i])){
    meta2$`Survey End Time`[i] <- paste0(meta2$`Recovery Date`[i],' 06:30:00')
  }else{
    if(meta2$`Recording Stop Time`[i] < 1 &
       meta2$`Recording Stop Time`[i] > 0){
      meta2$`Survey End Time`[i] <- paste(meta2$`Recovery Date`[i],times(as.numeric(meta2$`Recording Stop Time`[i])))
    }else{
      if(str_detect(meta2$`Recording Stop Time`[i],"\\+\\d{2}:\\d{2}")){
        tmp1 <- tz_lookup_coords(meta2$Latitude[i],meta2$Longitude[i],method = 'accurate')
        meta2$`Survey End Time`[i] <- 
          format(sunrise(as_datetime(meta2$`Recovery Date`[i],tz=tmp1),meta2$Longitude[i],meta2$Latitude[i],tz=tmp1) + 
                   lubridate::hours(as.numeric(str_extract(meta2$`Recording Stop Time`[i],"(?<=\\+)\\d{2}(?=:)"))) + 
                   lubridate::minutes(as.numeric(str_extract(meta2$`Recording Stop Time`[i],"(?<=:)\\d{2}$"))),"%Y-%m-%d %H:%M:%S")
      }else{NA}}}}

# Check Survey Start/End Times
meta2 %>% 
  select(`Deployment Date`,`Recovery Date`,`Recording Start Time`,`Recording Stop Time`,`Survey Start Time`,`Survey End Time`) %>% 
  arrange(`Survey Start Time`) %>% 
  print(n=Inf)

# Check Survey Start/End Times for long deployments (> 4 days)
meta2 %>% 
  select(LocationName,LocationName_GRTS,`Deployment Contact(s)`,`Deployment Date`,`Recovery Date`,`Recording Start Time`,`Recording Stop Time`,`Survey Start Time`,`Survey End Time`) %>% 
  mutate(duration = difftime(ymd_hms(`Survey End Time`),ymd_hms(`Survey Start Time`),units = 'days')) %>% 
  filter(duration > 4) %>% 
  arrange(desc(duration)) %>% 
  print(n=Inf)

# Check Survey Start/End Times for negative length deployments
meta2 %>% 
  select(LocationName,LocationName_GRTS,`Deployment Contact(s)`,`Deployment Date`,`Recovery Date`,`Recording Start Time`,`Recording Stop Time`,`Survey Start Time`,`Survey End Time`) %>% 
  mutate(duration = difftime(ymd_hms(`Survey End Time`),ymd_hms(`Survey Start Time`),units = 'days')) %>% 
  filter(duration < 0) %>% 
  arrange(desc(duration)) %>% 
  print(n=Inf)

names(meta2)
meta2 <- meta2 %>% 
  select(GRTS,
         LocationName,
         LocationName_GRTS,
         `New Site?`,
         Longitude,
         Latitude,
         `Survey Start Time`,
         `Survey End Time`,
         `Detector Type`,
         `Detector Serial No.`,
         `Microphone Orientation`,
         `Microphone Height (m)`,
         `Distance to Clutter (m)`,
         `Clutter Type`,
         `Waterbody Descriptor`,
         `Clutter Category`,
         `Habitat (choose one)`,
         `Deployment Contact(s)`,
         `Deployment Agency`,
         `Enter other Agency`,
         UnusualOccurrences)

### Combine Metadata ----
# Make sure both metadata files have the same columns
names(meta1)[!(names(meta1)%in%names(meta2))]
names(meta2)[!(names(meta2)%in%names(meta1))]

# Make data type consistent
meta1 <- meta1 %>% 
  mutate(`Detector Serial No.` = as.character(`Detector Serial No.`),
         `Microphone Height (m)` = as.character(`Microphone Height (m)`))

# Combine Field Maps and Paper Datasheets metadata
meta <- meta1 %>% 
  add_row(meta2)


### Remove non-NABat records ----
# Read in CellTracker
CellTracker <- read_excel(paste0("C:/Users/",onid,"/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_",year,".xlsx"))
# Identify all unique NABatProject values in CellTracker
CellTracker %>% 
  distinct(NABatProject)
# Look at all metadata that is not associated with an NABat Project
meta %>% 
  filter(!GRTS %in% (CellTracker %>% 
                      filter(NABatProject %in% c('NW','ID','USFWS','USFS_Region04','NW/USFWS')) %>% 
                      pull(GRTS_ID))) %>% 
  distinct(GRTS,`Deployment Contact(s)`) %>% 
  print(n=Inf)
# Use only metadata from valid NABat Projects
meta <- meta %>% 
  filter(GRTS %in% (CellTracker %>% 
                       filter(NABatProject %in% c('NW','ID','USFWS','USFS_Region04','NW/USFWS')) %>% 
                       pull(GRTS_ID)))



## Format metadata fields ----
### GRTS Cell ID ----
meta <- meta %>% 
  rename('GRTS Cell Id' = GRTS)

meta %>% 
  distinct(`GRTS Cell Id`) %>% 
  arrange(as.numeric(`GRTS Cell Id`)) %>% 
  print(n=Inf)

### Location Name ----
meta <- meta %>% 
  left_join(CellTracker %>% 
              select(GRTS_ID,NABatProject),
            by = c('GRTS Cell Id' = 'GRTS_ID')) %>% 
  mutate('Location Name' = 
           case_when(NABatProject %in% c('NW') ~ LocationName,
                     NABatProject %in% c('ID','USFWS','USFS_Region04') ~ LocationName_GRTS,
                     TRUE ~ NA))

# Check for records with Location Name not calculated
meta %>% 
  filter(is.na(`Location Name`)) %>% 
  select(`GRTS Cell Id`,LocationName,LocationName_GRTS,`Location Name`,`Deployment Agency`,`Deployment Contact(s)`,`Survey Start Time`)

# Make some corrections
meta <- meta %>% 
  mutate(NABatProject =
           case_when(LocationName %in% c('117633_NE1','117633_NW2','117633_SE1','117633_SW1') ~ 'NW',
                     LocationName %in% c('117633_NE2','117633_NE3','117633_NE4','117633_NE5') ~ 'USFWS',
                     TRUE ~ NABatProject),
         `Location Name` =
           case_when(LocationName %in% c('117633_NE1','117633_NW2','117633_SE1','117633_SW1') ~ LocationName,
                     LocationName %in% c('117633_NE2','117633_NE3','117633_NE4','117633_NE5') ~ LocationName_GRTS,
                     TRUE ~ `Location Name`)) %>% 
  filter(!is.na(`Location Name`))

# Identify Station Locations surveyed multiple times
meta %>% 
  filter(`Location Name` %in% (meta %>% 
                                 group_by(`Location Name`) %>% 
                                 count() %>% 
                                 filter(n > 1) %>% 
                                 pull(`Location Name`))) %>% 
  select(`GRTS Cell Id`,`Location Name`,`Survey Start Time`) %>% 
  arrange(`Location Name`,`Survey Start Time`)

# Add SiteDeployment to identify Sites surveyed multiple times through the season. 
meta <- meta %>% 
  mutate(SiteDeployment = 
           case_when(`Location Name` == '106997_SE1' &
                       str_detect(`Survey Start Time`,'2023-08-22') ~ 2,
                     `Location Name` == '110715_NW1' &
                       str_detect(`Survey Start Time`,'2023-07-11') ~ 2,
                     `Location Name` == '110715_SW1' &
                       str_detect(`Survey Start Time`,'2023-07-11') ~ 2,
                     `Location Name` == '113007_SW1' &
                       str_detect(`Survey Start Time`,'2023-08-03') ~ 2,
                     `Location Name` == '131055_SW1' &
                       str_detect(`Survey Start Time`,'2023-08-31') ~ 2,
                     `Location Name` == '15834_NE1' &
                       str_detect(`Survey Start Time`,'2023-07-06') ~ 2,
                     TRUE ~ 1))

# Check to make sure multiple deployments are properly documented ###
tmp1 <- meta %>% 
  group_by(`Location Name`) %>% 
  count() %>% 
  filter(n>1) %>% 
  pull(`Location Name`)

meta %>% 
  filter(`Location Name` %in% tmp1) %>% 
  select(`Location Name`,`Survey Start Time`,SiteDeployment) %>% 
  arrange(`Location Name`,SiteDeployment) %>% 
  print(n=Inf)

### Latitude and Longitude ----
# Only needed for new Idaho, USFWS, and USFS_Region4 Station Locations

# Pull existing locations from NW Bat Hub Database
# Need to be using 32-bit R for database connection
#con <- dbConnect(odbc::odbc(),
#                .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/schminad/Downloads/PNW_BatHub_Database.accdb")
#tblPointLocation <- dbReadTable(con,'tblPointLocation')

# Pull existing Locations from NABat Partner Portal
library(nabatr)

username <- readline(prompt = "Enter your email address used to log into NABat Partner Portal: ")
#token = get_nabat_gql_token(username)

# Alternatively, you can copy/paste your token from the NABat Partner Portal into the line below. Token is different everytime. Must login to Portal.
#token = list(refresh_token = 'eyJhbGciOiJIUzI1NiIsInR5cCIgOiAiSldUIiwia2lkIiA6ICJkMmMyMmZmMy1mZjIxLTQ4NjQtYjJlMS04NTQ1ZWJlNmU0NjkifQ.eyJleHAiOjE3MDMxODQ3NDAsImlhdCI6MTcwMzE4MDc4MCwianRpIjoiMDE5ZTU3ZjktNjg3ZC00NTk1LTgyZTUtYjgwODg0MTU2MmMzIiwiaXNzIjoiaHR0cHM6Ly93d3cuc2NpZW5jZWJhc2UuZ292L2F1dGgvcmVhbG1zL1NjaWVuY2VCYXNlIiwiYXVkIjoiaHR0cHM6Ly93d3cuc2NpZW5jZWJhc2UuZ292L2F1dGgvcmVhbG1zL1NjaWVuY2VCYXNlIiwic3ViIjoiYzBmNWZiNzMtNmZiMy00MDhkLWJiOWYtZGU2MGUwN2VjYzUzIiwidHlwIjoiUmVmcmVzaCIsImF6cCI6Im5hYmF0Iiwibm9uY2UiOiIzNzRhYjUyNi1iOTQ3LTQ4NjQtODQ1Ni1hMzA5ZGRhOWNhMTUiLCJzZXNzaW9uX3N0YXRlIjoiOWMwZDkwZTUtZDc2OC00MGM4LWFhYzYtNWU5NDk2ZmM5ZDEyIiwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBlbWFpbCJ9.-c1IV7FCCjRQxNhSmyYlYkJlMFyvstjrMZFMEtvQSo8', access_token = '', refresh_at = Sys.time())
token = list(refresh_token = 'eyJhbGciOiJIUzI1NiIsInR5cCIgOiAiSldUIiwia2lkIiA6ICJkMmMyMmZmMy1mZjIxLTQ4NjQtYjJlMS04NTQ1ZWJlNmU0NjkifQ.eyJleHAiOjE3MDc3NzYyOTUsImlhdCI6MTcwNzc3MjMzNSwianRpIjoiMTgxMzI0NjMtNzkzYy00OWU0LTg0Y2ItOWQ3OTI4MDNkZjI1IiwiaXNzIjoiaHR0cHM6Ly93d3cuc2NpZW5jZWJhc2UuZ292L2F1dGgvcmVhbG1zL1NjaWVuY2VCYXNlIiwiYXVkIjoiaHR0cHM6Ly93d3cuc2NpZW5jZWJhc2UuZ292L2F1dGgvcmVhbG1zL1NjaWVuY2VCYXNlIiwic3ViIjoiYzBmNWZiNzMtNmZiMy00MDhkLWJiOWYtZGU2MGUwN2VjYzUzIiwidHlwIjoiUmVmcmVzaCIsImF6cCI6Im5hYmF0Iiwibm9uY2UiOiJiMmUxYjhlNS1iNGEzLTRiMjEtOGQ4Yi1mZTAwM2JmNDU1ZDQiLCJzZXNzaW9uX3N0YXRlIjoiOTBlZDM5MWItNmZkNy00MTcwLTliZmUtYTQ2ZWFmMTA3YjRjIiwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBlbWFpbCJ9.rXrcR3Lp9HNZZm6pzEpvO3ZS11swGdJE-fW38bbUMLU', access_token = '', refresh_at = Sys.time())

# pull all existing Idaho NABat Station Locations
token = get_refresh_token(token)
project_df = get_projects(token)

project_df %>% 
  distinct(project_id,project_name) %>% 
  filter(str_detect(project_name,'Idaho'))
project_id = 48 # Set equal to one of your project ids
sa_survey_df = get_sa_project_summary(token, 
                                      project_df, 
                                      project_id)
IdahoLocations <- unique(sa_survey_df$event)

# pull all existing USFWS NABat Station Locations
token = get_refresh_token(token)
project_df = get_projects(token)
project_df %>% 
  distinct(project_id,project_name) %>% 
  filter(str_detect(project_name,'National Wildlife Refuges'))
project_id = 38 # Set equal to one of your project ids
sa_survey_df = get_sa_project_summary(token, 
                                      project_df, 
                                      project_id)
USFWSLocations <- unique(sa_survey_df$event)

# pull all existing USFS_Region4 NABat Station Locations
token = get_refresh_token(token)
project_df = get_projects(token)
project_df %>% 
  distinct(project_id,project_name) %>% 
  filter(str_detect(project_name,'Region 4'))
project_id = 1070 # Set equal to one of your project ids
sa_survey_df = get_sa_project_summary(token, 
                                      project_df, 
                                      project_id)
USFSR4Locations <- unique(sa_survey_df$event)

# Look at metadata for Locations not in database, check any that show New Site = No
meta %>% 
  filter(NABatProject %in% c('ID','USFWS','USFS_Region04') & 
           !`Location Name` %in% c(IdahoLocations,USFWSLocations,USFSR4Locations) &
           `New Site?` != 'Yes') %>% 
  select(`GRTS Cell Id`,`Location Name`,`New Site?`,NABatProject) %>% 
  arrange(NABatProject,`Location Name`) %>% 
  print(n=Inf)

meta %>% 
  filter(NABatProject %in% c('ID','USFWS','USFS_Region04') & 
           !`Location Name` %in% c(IdahoLocations,USFWSLocations,USFSR4Locations))

meta <- meta %>% 
  mutate(Latitude = 
           case_when(NABatProject %in% c('ID','USFWS','USFS_Region04') & 
                       !`Location Name` %in% c(IdahoLocations,USFWSLocations,USFSR4Locations) ~ Latitude,
                     TRUE ~ NA_real_),
         Longitude = 
           case_when(NABatProject %in% c('ID','USFWS','USFS_Region04') & 
                       !`Location Name` %in% c(IdahoLocations,USFWSLocations,USFSR4Locations) ~ Longitude,
                     TRUE ~ NA_real_))

meta %>% 
  filter(!is.na(Latitude)) %>% 
  dim()


### Survey Start Time ----
# Washington Start Times are quite variable
# It looks like WDFW often set Detectors to Sunset - 00:30 through Sunrise + 00:30, instead of standard 19:30 - 06:30
# ODFW often does not enter correct Recording Start/Stop Times

# List Recording Start Times
meta %>% 
  distinct(time = str_extract(`Survey Start Time`, '\\d{2}:\\d{2}:\\d{2}')) %>% 
  arrange(time) %>% 
  print(n=Inf)

### Look into odd Start Times
meta %>% 
  filter(str_detect(`Survey Start Time`, '22:30:00')) %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Deployment Contact(s)`,`Survey Start Time`)

meta %>% 
  filter(`GRTS Cell Id`== 79935) %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Deployment Contact(s)`,`Detector Type`,`Survey Start Time`)

# Check Log file and Raw acoustic data to try to confirm Survey Start Time
# Make necessary corrections in metadata


### Survey End Time ----
# List Survey End Times
meta %>% 
  distinct(time = str_extract(`Survey End Time`, '\\d{2}:\\d{2}:\\d{2}')) %>% 
  arrange(time) %>% 
  print(n=Inf)

# Look into odd End Times
meta %>% 
  filter(str_detect(`Survey End Time`, '12:06:00')) %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Deployment Contact(s)`,`Survey End Time`)

meta %>% 
  filter(`GRTS Cell Id`==2266) %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Deployment Contact(s)`,`Detector Type`,`Survey End Time`)

# Check Log file and Raw acoustic data to try to confirm Survey Start Time
# Make necessary corrections in metadata

# Check deployment duration
# Negatives are obviously erroneous, go back and make corrections in metadata
meta %>% 
  mutate(dur = ymd_hms(`Survey End Time`) - ymd_hms(`Survey Start Time`)) %>% 
  select(`Location Name`, dur,`Survey Start Time`,`Survey End Time`) %>% 
  filter(dur < 8) %>% 
  print(n=Inf)

### 2023 Notes for negative deployment duration
# 103755_NE1, Deployment failure
# 105134_NE1, Deployment failure

### Detector ----
meta %>%
  distinct(`Detector Type`)

# Make corrections for NAs
meta %>% 
  filter(is.na(`Detector Type`)) %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`)

meta <- meta %>% 
  mutate(Detector = 
           case_when(`Detector Type` == 'D500X' | 
                       `Detector Type` == 'D500x' ~ 'PETTERSSON D500x',
                     `Detector Type` == 'SM4BAT' ~ 'WILDLIFE ACOUSTICS SM4BAT-FS',
                     `Detector Type` == 'SM3BAT' ~ 'WILDLIFE ACOUSTICS SM3Bat',
                     `Detector Type` == 'SM2' ~ 'WILDLIFE ACOUSTICS SM2Bat+',
                     `Detector Type` == 'SMMiniBat' | 
                       `Detector Type` == 'Song Meter Mini Bat' ~ 'WILDLIFE ACOUSTICS SMMINI-BAT',
                     `Detector Type` == 'Anabat Swift' |
                       `Detector Type` == 'AnaSwift' |
                       `Detector Type` == 'Titley Scientific Swift' ~ 'TITLEY AnaBat Swift',
                     `Detector Type` ==  'AnaExpress' |
                       `Detector Type` == 'Anabat Express' ~ 'TITLEY AnaBat Express',
                     TRUE ~ 'check'))
meta %>% 
  distinct(`Detector Type`,Detector)


### Detector Serial Number ----
# Use DetectorCheck to confirm Serial Number
# Copy all DetectorCheck.csv files from the BatHub Server to Box
detOR <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/DetectorCheck_Oregon_',year,'.csv'),col_types = cols(.default='c'))
detWA <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/DetectorCheck_Washington_',year,'.csv'),col_types = cols(.default='c'))
detID <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/DetectorCheck_Idaho_',year,'.csv'),col_types = cols(.default='c'))
detUSFWS <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/DetectorCheck_USFWS_',year,'.csv'),col_types = cols(.default='c'))
detUSFS4 <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/DetectorCheck_USFS_Region4_',year,'.csv'),col_types = cols(.default='c'))

DetectoCheck <- detOR %>% 
  add_row(detWA) %>% 
  add_row(detID) %>% 
  add_row(detUSFWS) %>% 
  add_row(detUSFS4)

DetectoCheck <- DetectoCheck %>% 
  mutate('Location Name' =
           case_when(Project %in% c('OR','WA') ~ LocationNameCONUS,
                     TRUE ~ LocationNameGRTS)) %>% 
  select(GRTS,`Location Name`,SiteDeployment,Files,fileDetectorSN)

meta <- meta %>% 
  mutate(`GRTS Cell Id` = as.character(`GRTS Cell Id`),
         SiteDeployment = as.character(SiteDeployment)) %>% 
  left_join(DetectoCheck, 
            by = c('GRTS Cell Id' = 'GRTS',
                   'Location Name',
                   'SiteDeployment'))

# Look at records with no fileDetectorSN
meta %>% 
  filter(is.na(fileDetectorSN)) %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Survey Start Time`,`Detector Type`,`Detector Serial No.`,fileDetectorSN,Files) %>% 
  print(n=Inf)

# Look at records with fileDetectorSN error
meta %>% 
  filter(fileDetectorSN=='ERROR') %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Survey Start Time`,`Detector Type`,`Detector Serial No.`,fileDetectorSN,Files) %>% 
  print(n=Inf)

# Look at records where metadata SN does not match file SN
meta %>% 
  filter(!str_detect(fileDetectorSN,`Detector Serial No.`) &
           fileDetectorSN != 'ERROR') %>%
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Survey Start Time`,`Detector Type`,`Detector Serial No.`,fileDetectorSN,Files) %>% 
  print(n=Inf)


# Assign Detector Serial Number from DetectorCheck, unless ERROR or NA
meta <- meta %>% 
  mutate('Detector Serial Number' = 
           case_when(is.na(fileDetectorSN) |
                       fileDetectorSN == 'ERROR' ~ `Detector Serial No.`,
                     TRUE ~ fileDetectorSN))

meta %>%
  group_by(Detector, `Detector Serial Number`) %>% 
  count() %>% 
  print(n=Inf)

# Look at incomplete serial numbers
meta %>% 
  filter((Detector == 'PETTERSSON D500x' &
           nchar(`Detector Serial Number`) != 5) |
           (Detector == 'WILDLIFE ACOUSTICS SM4BAT-FS' &
              nchar(`Detector Serial Number`) != 8) |
           (Detector == 'TITLEY AnaBat Swift' &
              nchar(`Detector Serial Number`) != 6) |
           (Detector == 'WILDLIFE ACOUSTICS SM3Bat' &
              nchar(`Detector Serial Number`) != 8) |
           (Detector == 'TITLEY AnaBat Express' &
              nchar(`Detector Serial Number`) != 6) |
           (Detector == 'WILDLIFE ACOUSTICS SMMINI-BAT' &
              nchar(`Detector Serial Number`) != 8)) %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,Detector,`Detector Serial Number`) %>% 
  arrange(Detector,`Detector Serial Number`)

# fix incomplete serial numbers
meta <- meta %>% 
  mutate(`Detector Serial Number` =
           case_when(Detector == 'PETTERSSON D500x' &
                       nchar(`Detector Serial Number`) != 5 ~ str_pad(`Detector Serial Number`,5,side = 'left',pad = '0'),
                     Detector == 'WILDLIFE ACOUSTICS SM4BAT-FS' &
                       nchar(`Detector Serial Number`) != 8 ~ paste0('S4U',str_pad(str_extract(`Detector Serial Number`,'\\d+$'),5,side='left',pad='0')),
                     Detector == 'WILDLIFE ACOUSTICS SMMINI-BAT' &
                       nchar(`Detector Serial Number`) != 8 ~ paste0('SMU',str_pad(str_extract(`Detector Serial Number`,'\\d+$'),5,side='left',pad='0')),
                     TRUE ~ `Detector Serial Number`))

# Look at all unique Detector and Serial Number values for anything weird
meta %>% 
  distinct(Detector,`Detector Serial Number`) %>% 
  arrange(Detector,`Detector Serial Number`) %>% 
  print(n=Inf)

# Look at NA Serial Numbers
meta %>% 
  filter(is.na(`Detector Serial Number`)) %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Survey Start Time`,`Detector Type`,`Detector Serial No.`,fileDetectorSN,Detector,`Detector Serial Number`)


### Microphone ----
meta %>% 
  distinct(Detector)

meta <- meta %>% 
  mutate(Microphone = 
           case_when(Detector == 'PETTERSSON D500x' ~ 'Pettersson D500x',
                     Detector == 'WILDLIFE ACOUSTICS SM4BAT-FS' ~ 'Wildlife Acoustics SMM-U2',
                     Detector == 'WILDLIFE ACOUSTICS SM3Bat' ~ 'Wildlife Acoustics SMM-U2',
                     Detector == 'WILDLIFE ACOUSTICS SM2Bat+' ~ 'Wildlife Acoustics SMX-U1',
                     Detector == 'WILDLIFE ACOUSTICS SMMINI-BAT' ~ 'generic Omni-directional',
                     Detector == 'TITLEY AnaBat Swift' ~ 'TITLEY AnaBat Swift',
                     Detector == 'TITLEY AnaBat Express' ~ 'generic Omni-directional',
                     TRUE ~ 'Error'))
meta %>% 
  distinct(Microphone)


### Microphone Orientation ----
meta %>% 
  distinct(`Microphone Orientation`)

meta <- meta %>% 
  mutate('Microphone Orientation' = 
           case_when(`Microphone Orientation` == 'N' ~ 'n',
                     `Microphone Orientation` == 'E' ~ 'e',
                     `Microphone Orientation` == 'S' ~ 's',
                     `Microphone Orientation` == 'W' ~ 'w',
                     `Microphone Orientation` == 'NE' ~ 'ne',
                     `Microphone Orientation` == 'SE' ~ 'se',
                     `Microphone Orientation` == 'SW' ~ 'sw',
                     `Microphone Orientation` == 'NW' ~ 'nw',
                     TRUE ~ NA_character_))
meta %>% 
  distinct(`Microphone Orientation`)

meta %>% 
  filter(is.na(`Microphone Orientation`))


### Microphone Height (meters) ----
meta %>% 
  distinct(`Microphone Height (m)`) %>% 
  arrange(`Microphone Height (m)`)

# Check records that look like they were probably recorded in feet, not meters
# Confirm with SitePhotos
# Go back and make corrections in metadata
meta %>% 
  filter(`Microphone Height (m)` >= 5) %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Deployment Contact(s)`,`Microphone Height (m)`)


### Assign values to Microphone Height (meters)
meta <- meta %>% 
  mutate('Microphone Height (meters)' = 
           case_when(!is.na(`Microphone Height (m)`) ~ `Microphone Height (m)`,
                     TRUE ~ NA_character_))

meta %>% 
  distinct(`Microphone Height (meters)`) %>% 
  arrange(`Microphone Height (meters)`)


### Distance to Nearest Clutter (meters) ----
meta %>% 
  distinct(`Distance to Clutter (m)`) %>% 
  arrange(`Distance to Clutter (m)`) %>% 
  print(n=Inf)


meta <- meta %>% 
  mutate('Distance to Nearest Clutter (meters)' = `Distance to Clutter (m)`)

meta %>% 
  distinct(`Distance to Nearest Clutter (meters)`) %>% 
  arrange(`Distance to Nearest Clutter (meters)`) %>% 
  print(n=Inf)


### Clutter Type ----
meta %>% 
  distinct(`Clutter Type`)

meta <- meta %>% 
  mutate('Clutter Type' = 
           case_when(`Clutter Type` == 'Vegetation' ~ 'Vegetation',
                     `Clutter Type` == 'WaterSource' | 
                       `Clutter Type` == 'Water Source' |
                       `Clutter Type` == 'Water' ~ 'Water',
                     `Clutter Type` == 'RockFeature' | 
                       `Clutter Type` == 'Rock Feature' | 
                       `Clutter Type` == 'Rock' ~ 'Rock',
                     `Clutter Type` == 'Building' ~ 'Building',
                     str_detect(`Clutter Type`, 'Other') ~ 'Other',
                     `Clutter Type` == 'None' |
                       is.na(`Clutter Type`) |
                       `Clutter Type` == '' ~ NA_character_,
                     TRUE ~ 'Error'))

meta %>% 
  distinct(`Clutter Type`)


### Water Type ----
meta %>% 
  distinct(`Waterbody Descriptor`)
### Descriptors used 2020
#guzzler
#lake
#pond
#river
#spring
#stock tank/trough
#stream
#tank
#wetland
#other

### Field Maps options
#river
#stream
#spring
#lake
#pond
#wetland
#stock tank/trough

### Look at Site Photos to confirm any confusing descriptors or to select dominant feature if multiple identified.
meta %>% 
  filter(`Waterbody Descriptor` == 'river; wetland')

meta <- meta %>% 
  mutate(`Water Type` = 
           case_when(`Waterbody Descriptor` == 'stocktank/trough' ~ 'stock tank/trough',
                     str_detect(`Waterbody Descriptor`,'other') ~ 'other',
                     str_detect(`Waterbody Descriptor`,'pond') ~ 'pond',
                     str_detect(`Waterbody Descriptor`,'river') ~ 'river',
                     `Waterbody Descriptor` == 'NA' ~ NA_character_,
                     TRUE ~ `Waterbody Descriptor`))
meta %>%
  distinct(`Water Type`)


### Percent Clutter ----
meta %>% 
  distinct(`Clutter Category`)

meta <- meta %>% 
  mutate('Percent Clutter' = 
           case_when(`Clutter Category` == 0 ~ '0',
                     `Clutter Category` == 1 ~ '25',
                     `Clutter Category` == 2 ~ '50',
                     `Clutter Category` == 3 ~ '75',
                     `Clutter Category` == 4 ~ '100',
                     TRUE ~ NA_character_))
meta %>% 
  distinct(`Percent Clutter`)

meta %>% 
  filter(is.na(`Percent Clutter`))


### Broad Habitat Type ----
# agriculture
# barren land
# forest-conifer
# forest-deciduous
# forested wetland
# grassland
# shrubland
# urban
# water
# wetland
meta %>% 
  distinct(`Habitat (choose one)`)

# Since habitat is subjective investigate all instances of multiple selections and 'others' before using the below categorization
# Use AGOL, Living Atlas, USA NLCD Land Cover layer and Google Maps satellite imagery to help identify broad habitat and surrounding forest type. 
meta %>% 
  filter(!(`Habitat (choose one)` %in% c('agriculture',
                                         'barren land',
                                         'forest-conifer','mixedconifer','mixed conifer','dryconifer','dry conifer','conifer','alpineforest','alpine forest',
                                         'forest-deciduous','hardwood',
                                         'forested wetland','mesicforest','mesic forest',
                                         'grassland',
                                         'shrubland','shrub-stepp','shrub-steppe',
                                         'urban',
                                         'water',
                                         'wetland'))) %>% 
  distinct(`Habitat (choose one)`)


### Investigate non-standard habitat
### Tibbles print numbers with 3 sig fig by default
### Define options, pillar::pillar_options
old <- options(pillar.sigfig = 9)
meta %>% 
  filter(`Habitat (choose one)` == 'dry meadow') %>% 
  select(NABatProject,`Location Name`,Latitude,Longitude)
# Restore old options
options(old)


meta <- meta %>% 
  mutate('Broad Habitat Type' =
           case_when(`Habitat (choose one)` == 'mixedconifer' |
                       `Habitat (choose one)` == 'mixed conifer' |
                       `Habitat (choose one)` == 'dryconifer' |
                       `Habitat (choose one)` == 'dry conifer' |
                       `Habitat (choose one)` == 'Burned Area' |
                       `Habitat (choose one)` == 'conifer' |
                       `Habitat (choose one)` == 'alpineforest' |
                       `Habitat (choose one)` == 'alpine forest' |
                       `Habitat (choose one)` == 'High Elevation Forest' |
                       `Habitat (choose one)` == 'Burned Forest' |
                       `Habitat (choose one)` == 'burned area' |
                       `Habitat (choose one)` == 'dry meadow' |
                       `Habitat (choose one)` == 'wet meadow' ~ 'forest-conifer',
                     `Habitat (choose one)` == 'mesicforest' |
                       `Habitat (choose one)` == 'mesic forest' ~ 'forested wetland',
                     `Habitat (choose one)` == 'hardwood' ~ 'forest-deciduous',
                     `Habitat (choose one)` == 'shrub-stepp' |
                       `Habitat (choose one)` == 'shrub-steppe' ~ 'shrubland',
                     `Habitat (choose one)` == 'playa' ~ 'barren land',
                     TRUE ~ `Habitat (choose one)`))


meta %>% 
  distinct(`Broad Habitat Type`)

meta %>% 
  filter(is.na(`Broad Habitat Type`))


### Contact ----
meta %>% 
  distinct(`Deployment Agency`)

meta %>% 
  filter(`Deployment Agency`=='Other') %>% 
  select(NABatProject,`GRTS Cell Id`,`Location Name`,`Deployment Contact(s)`,`Deployment Agency`,`Enter other Agency`) %>% 
  print(n=Inf)

meta <- meta %>% 
  mutate(Contact =
           case_when(`Deployment Agency` == 'OSU' ~ 'Oregon State University',
                     `Deployment Agency` == 'USFS' ~ 'US Forest Service',
                     `Deployment Agency` == 'USGS' ~ 'US Geological Survey',
                     `Deployment Agency` %in% c('USFWS','FWS') ~ 'US Fish and Wildlife Service',
                     `Deployment Agency` == 'BLM' ~ 'Bureau of Land Management',
                     `Deployment Agency` == 'NPS' ~ 'National Park Service',
                     `Deployment Agency` == 'ODFW' ~ 'Oregon Department of Fish and Wildlife',
                     `Deployment Agency` == 'WDFW' ~ 'Washington Department of Fish and Wildlife',
                     `Deployment Agency` == 'IDFG' ~ 'Idaho Fish and Game',
                     `Deployment Agency` %in% c('Kalispel Tribe','KTI') ~ 'Kalispel Tribe of Indians',
                     `Deployment Agency` == 'WPZ' ~ 'Woodland Park Zoo',
                     `Deployment Agency` %in% c('Veolia/INL','INL') ~ 'Idaho National Laboratory',
                     `Deployment Agency` == 'Other' &
                       `Enter other Agency` == 'TNC' ~ 'The Nature Conservancy',
                     `Deployment Agency` == 'Other' & 
                       `Enter other Agency` %in% c('Idaho Army National Guard','IDARNG') ~ 'Idaho Army National Guard',
                     `Deployment Agency` == 'Other' &
                       `Enter other Agency` == 'Oregon Military Department' ~ 'Oregon Military Department',
                     `Deployment Agency` == 'Other' &
                       `Deployment Contact(s)` %in% c('Jenna Chapman','Jenna chapman') ~ 'Yakima Training Center',
                     `Deployment Agency` == 'Other' &
                       `Deployment Contact(s)` == 'Kelsey Lotz' ~ 'Hanford Mission Integration Solutions',
                     `Deployment Agency` == 'Other' &
                       `Deployment Contact(s)` == 'Katie Remine' ~ 'Woodland Park Zoo',
                     `Deployment Agency` == 'Other' &
                       `Enter other Agency` == 'Idaho Master Naturalist' ~ 'Idaho Master Naturalist',
                     TRUE ~ `Deployment Agency`))
meta %>% 
  distinct(Contact)

## Clean prepped metadata ----
names(meta)
meta <- meta %>% 
  select(NABatProject,
         `GRTS Cell Id`,
         `Location Name`,
         Latitude,
         Longitude,
         SiteDeployment,
         `Survey Start Time`,
         `Survey End Time`,
         Detector,
         `Detector Serial Number`,
         Microphone,
         `Microphone Orientation`,
         `Microphone Height (meters)`,
         `Distance to Nearest Clutter (meters)`,
         `Clutter Type`,
         `Water Type`,
         `Percent Clutter`,
         `Broad Habitat Type`,
         Contact,
         UnusualOccurrences)


## Save Clean Metadata ----
write_csv(meta, paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABat_CleanMetadata_',year,'.csv', na = ''))
