library(tidyverse)
library(readxl)
library(lubridate)


### Identify yourself, the Project, and year of data to check
onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
project <- readline(prompt = "Enter the Project/State corresponding to the acoustic data directory you want to check: {Oregon | Washington | Idaho | USFWS | USFS_Region4}")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")

setwd(paste0("E:/NABat/", project, "/Raw/",year))


### Read in existing Vetting Tracker
VettingTracker <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_BatAcousticFiles/NABat/VettingTracker',year,'.xlsx'))


##################################################
### List all paths to sites within a directory ###
##################################################
### First list all directories
a <- list.dirs(full.names = F)

### Extract path to quad folder
quad.folders <- str_extract(a, '.*\\d+_.+\\d[^/]*')
quad.folders <- na.omit(unique(quad.folders))

data.acoustic <- data.frame(State = project,
                            Path = paste0(getwd(), '/', quad.folders))

CellList <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx'), col_types = c('text')) %>% 
  select(1:2)


data.acoustic <- data.acoustic %>% 
  mutate(SU = str_extract(Path, '\\d+(?=_)')) %>% 
  left_join(CellList, by = c('SU' = 'CONUS_10KM')) %>% 
  left_join(CellList, by = c('SU' = 'GRTS_ID')) %>% 
  mutate(CONUS =
           case_when(project %in% c('Oregon','Washington') ~ SU,
                     TRUE ~ CONUS_10KM),
         GRTS = 
           case_when(project %in% c('Idaho','USFWS','USFS_Region4') ~ SU,
                     TRUE ~ GRTS_ID),
         LocationNameCONUS = paste0(CONUS,str_extract(Path, '_[NS][EW]\\d')),
         LocationNameGRTS = paste0(GRTS,str_extract(Path, '_[NS][EW]\\d')),
         SiteDeployment = 1,
         filesRecorded = NA_integer_,
         filesScrubbed = NA_integer_,
         filesAutoClassified = NA_integer_,
         filesBatID = NA_integer_) %>% 
  select(-c(3:5))

### Check to make sure all Processed folders have corresponding data.acoustic$Site
setwd(paste0("E:/NABat/", project, "/Processed/",year))
b <- list.dirs(full.names = F)
proc <- paste0(getwd(),'/',b)
b <- na.omit(unique(str_extract(b, '\\d+_[NS][EW]\\d')))

if(project %in% c('Oregon','Washington')){
  b[!(b %in% data.acoustic$LocationNameCONUS)]
}else{b[!(b %in% data.acoustic$LocationNameGRTS)]}

if(project %in% c('Oregon','Washington')){
  data.acoustic[!(data.acoustic$LocationNameCONUS %in% b),]
}else{data.acoustic[!(data.acoustic$LocationNameGRTS %in% b),]}

### 2023 Notes
### 105579_NE4 has all Noise files, only 1 file recorded
### 112067_NE1 has all Noise files, only 3 files recorded
### 119062_SW1 has all Noise files, lots of static in acoustic files
### 119480_SW1 has all Noise files, only 6 files recorded
### 120870_SE2 has all Noise files, files appear corrupted and cannot be restored
### All 52334 deployments until 8/12/2023 are all Noise, file length = 00:00:00

##############################################
### Check for Sites sampled multiple times ###
##############################################
data.acoustic %>% 
  group_by(LocationNameCONUS) %>% 
  count() %>% 
  filter(n>1)


### Check if all second deployments have (2) in Path
data.acoustic %>% 
  filter(str_detect(.$Path, '\\(2\\)'))

#### Set deployment for the obvious ones
data.acoustic <- data.acoustic %>% 
  mutate(SiteDeployment = 
           case_when(str_detect(.$Path, '\\(2\\)') ~ 2,
                     TRUE ~ 1))


### make data.acoustic only new data going forward
data.acoustic <- data.acoustic %>% 
  anti_join(VettingTracker,
            by = c('LocationNameCONUS', 'SiteDeployment'))


####################################################
### Count number of acoustic files for each Site ###
####################################################
### BatIDs in processed data
bats <- c("Anpa",
          "Coto",
          "Epfu",
          "Euma",
          "Laci",
          "Lano",
          "Myca", 
          "Myci",
          "Myev",
          "Mylu",
          "Myth",
          "Myvo",
          "Myyu",
          "Pahe",
          "Tabr")


for(i in 1:length(data.acoustic$Path)){
  if(str_detect(data.acoustic$Path[i], 'ZeroCrossing')){
    tmp1 <- read.csv(paste0('E:/NABat/AcousticData/Idaho/Processed/',year,'/KaleidoscopeOutput/', data.acoustic$GRTS[i], '_ZC/id.csv'))
    tmp1$LocationNameGRTS <- str_extract(tmp1$FOLDER, '\\d+_[NS][EW]\\d')
    tmp1$LocationNameCONUS <- paste0(data.acoustic$CONUS[i],str_extract(tmp1$LocationNameGRTS,'_[NS][EW]\\d'))
    tmp2 <- tmp1[tmp1$LocationNameCONUS == data.acoustic$LocationNameCONUS[i],]
    data.acoustic$filesRecorded[i] <- length(tmp2$AUTO.ID.)
    data.acoustic$filesScrubbed[i] <- length(tmp2$AUTO.ID.[!(tmp2$AUTO.ID. %in% c('Noise'))])
    data.acoustic$filesBatID[i] <- length(tmp2$AUTO.ID.[!(tmp2$AUTO.ID. %in% c('Noise', 'NoID'))])
    data.acoustic$Path[i] <- paste0('E:/NABat/AcousticData/Idaho/Processed/2021/KaleidoscopeOutput/', data.acoustic$GRTS[i], '_ZC')
  }else{
  setwd(data.acoustic$Path[i])
  data.acoustic$filesRecorded[i] <- length(list.files(pattern = '.wav', ignore.case = T, recursive = T))
  data.acoustic$filesScrubbed[i] <- length(list.files(pattern = '.wav', ignore.case = T, recursive = F))
  if(project %in% c('Oregon','Washington')){
    tmp1 <- data.acoustic$LocationNameCONUS[i]
    tmp2 <- data.acoustic$SiteDeployment[i]
  }else{
    tmp1 <- data.acoustic$LocationNameGRTS[i]
    tmp2 <- data.acoustic$SiteDeployment[i]}
  if(dir.exists(str_replace(as.character(data.acoustic$Path[i]), 'Raw', 'Processed'))){
  setwd(str_replace(as.character(data.acoustic$Path[i]), 'Raw', 'Processed'))
  tmp1 <- list.files(pattern = '.wav', ignore.case = T)
  tmp2 <- str_extract(tmp1, '(?<=-)[^\\d]+(?=\\.)')
  data.acoustic$filesAutoClassified[i] <- length(tmp1)
  data.acoustic$filesBatID[i] <- length(tmp2[tmp2 %in% bats])
  data.acoustic$Path[i] <- getwd()
  }else{data.acoustic$filesBatID[i] <- 0}
  }
}

head(data.acoustic)

###############################################################################################################################
### From metadata we need NewSite, DetectorType, DetectorSN, Deployment.Date, Recovery.Date, NoteAcoustic, and NoteScrubbed ###
###############################################################################################################################
#meta1 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/2022_Analysis/NABatSurveyForm2022_20220920.csv')
#meta1 <- read_csv(paste0('C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatSurveyForm',year,'.csv'))
#meta1 <- meta1 %>% 
  #mutate(LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`),
         #LocationNameGRTS = paste0(GRTS, '_', Quad, `Quad Number`),
         #DeploymentDate = as.Date(mdy_hms(`Deployment Date`), tz = 'America/Los_Angeles'),
         #RecoveryDate = as.Date(mdy_hms(`Recovery Date`), tz = 'America/Los_Angeles'),
         #DeploymentNights = RecoveryDate - DeploymentDate,
         #DetectorSN = as.character(`Detector Serial No.`)) %>% 
  #select(LocationNameCONUS,
         #LocationNameGRTS,
        #`New Site?`,
         #DeploymentDate,
        # RecoveryDate,
        # DeploymentNights,
         #`Detector Type`,
        # DetectorSN) %>% 
  #rename('New2023' = 'New Site?',
         #'DetectorType' = 'Detector Type')

### During the season, before Field Maps editing has been disabled and exported to NABatMetadata{year}_FieldMaps.xlsx, use NABatSurveyForm{year}.csv
if(file.exists(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_FieldMaps.xlsx'))){
  meta1 <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_FieldMaps.xlsx'))
}else{
  meta1 <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatSurveyForm',year,'.csv'))
}

# Format Field Maps metadata
# Ignore Warning: 'All formats failed to parse. No formats found.' as long as output DeploymentDate and RecoveryDate look good
meta1 <- meta1 %>% 
  mutate(LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`),
         LocationNameGRTS = paste0(GRTS, '_', Quad, `Quad Number`),
         DeploymentDate = 
           case_when(is.character(`Deployment Date`) ~ as.Date(mdy_hms(`Deployment Date`), tz = 'America/Los_Angeles'),
                     TRUE ~ as.Date(`Deployment Date`, tz = 'America/Los_Angeles')),
         RecoveryDate = 
           case_when(is.character(`Deployment Date`) ~ as.Date(mdy_hms(`Recovery Date`), tz = 'America/Los_Angeles'),
                     TRUE ~ as.Date(`Recovery Date`, tz = 'America/Los_Angeles')),
         DeploymentNights = RecoveryDate - DeploymentDate,
         DetectorSN = as.character(`Detector Serial No.`)) %>% 
  select(LocationNameCONUS,
         LocationNameGRTS,
         `New Site?`,
         DeploymentDate,
         RecoveryDate,
         DeploymentNights,
         `Detector Type`,
         DetectorSN) %>% 
  rename('New2023' = 'New Site?',
         'DetectorType' = 'Detector Type')

meta2 <- read_excel(paste0('C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_PaperDatasheets.xlsx'))
meta2 <- meta2 %>% 
  mutate(LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`),
         LocationNameGRTS = paste0(GRTS, '_', Quad, `Quad Number`),
         DeploymentDate = as.Date(`Deployment Date`),
         RecoveryDate = as.Date(`Recovery Date`),
         DeploymentNights = RecoveryDate - DeploymentDate) %>% 
  select(LocationNameCONUS,
         LocationNameGRTS,
         `New Site?`,
         DeploymentDate,
         RecoveryDate,
         DeploymentNights,
         `Detector Type`,
         `Detector Serial No.`) %>% 
  rename('New2023' = 'New Site?',
         'DetectorType' = 'Detector Type',
         'DetectorSN' = 'Detector Serial No.')


meta <- meta1 %>% 
  add_row(meta2)


meta <- meta %>% 
  filter(LocationNameCONUS %in% data.acoustic$LocationNameCONUS)


### Check to see if any Sites have multiple metadata records
meta %>% 
  group_by(LocationNameCONUS) %>% 
  count() %>% 
  filter(n > 1)

meta %>% 
  filter(LocationNameCONUS == '100524_NE1')

meta <- meta %>% 
  mutate(SiteDeployment = 
           case_when(LocationNameCONUS == '100524_NE1' &
                       DeploymentDate == '2023-07-06' ~ 2,
                     LocationNameCONUS == '106997_SE1' &
                       DeploymentDate == '2023-08-22' ~ 2,
                     LocationNameCONUS == '110715_NW1' &
                       DeploymentDate == '2023-07-11' ~ 2,
                     LocationNameCONUS == '110715_SW1' &
                       DeploymentDate == '2023-07-11' ~ 2,
                     LocationNameCONUS == '113007_SW1' &
                       DeploymentDate == '2023-08-03' ~ 2,
                     LocationNameCONUS == '131055_SW1' &
                       DeploymentDate == '2023-08-31' ~ 2,
                     TRUE ~ 1))

### Check if there are deployments without raw data folder
### We expect there should be some, from folks who use Field Maps, but we haven't gotten their data up yet. 
meta[!(meta$LocationNameCONUS %in% data.acoustic$LocationNameCONUS),]
### Or acoustic data without metadata
data.acoustic[!(data.acoustic$LocationNameCONUS %in% meta$LocationNameCONUS),]



### Read in classifier and format
classifier <- read_csv('C:/Users/schminad/Box/HERS_Working/Bats/DataManager/Scripts/ConusGrtsClassifier.csv') %>% 
  select(1:3)
names(classifier) <- c('CONUS','GRTS','Classifier')
classifier$CONUS <- as.character(classifier$CONUS)
classifier$GRTS <- as.character(classifier$GRTS)

### Add metadata and classifier to acoustic data
dim(data.acoustic)
dim(meta)
head(data.acoustic)
head(meta)

data.acoustic <- data.acoustic %>% 
  left_join(meta) %>% 
  left_join(classifier) %>% 
  select(
    State,
    Path,
    CONUS,
    GRTS,
    LocationNameCONUS,
    LocationNameGRTS,
    New2023,
    SiteDeployment,
    DeploymentDate,
    RecoveryDate,
    DeploymentNights,
    DetectorType,
    DetectorSN,
    Classifier,
    filesRecorded,
    filesScrubbed,
    filesAutoClassified,
    filesBatID)


#############################################
### Make sure all Sites have a classifier ###
#############################################
data.acoustic %>% 
  distinct(Classifier)


### Idaho Sites use Eastern Washington classifier
### Nevada Sites use Great Basin classifier
data.acoustic <- data.acoustic %>% 
  mutate(Classifier = 
           case_when(State == 'ID' ~ 'Eastern Washington',
                     State == 'NV' ~ 'Great Basin',
                     TRUE ~ Classifier))

data.acoustic %>% 
  filter(is.na(Classifier))

###################
### Trap Nights ###
###################
### To apply to previously created Vetting Tracker, read it in
#data.acoustic <- read.csv('C:/Users/emblidgp/Desktop/DetectorCheckOR.csv')
head(data.acoustic)

### Function converting timestamp to Trap Night based on time recorded
TrapNight <- function(time) {
  if(format(time, '%H:%M:%S') > '12:00:00'){
    return(as.character(as.Date(time)))
  }else{
    return(as.character(as.Date(time) -1))
  }
}

data.acoustic <- data.acoustic %>% 
  mutate(fileTrapNights = NA_integer_,
         fileFirst = NA_character_,
         fileLast = NA_character_,
         Note = NA_character_)


for(i in 1:length(data.acoustic$Path)){
  if(str_detect(data.acoustic$Path[i], 'Kaleidoscope')){
    setwd(data.acoustic$Path[i])
    tmp1 <- read.csv('id.csv')
    tmp1$Site <- str_extract(tmp1$FOLDER, '\\d+_[NS][EW]\\d')
    tmp2 <- tmp1 %>% 
      filter(Site == data.acoustic$Site[i] & AUTO.ID. != 'Noise')
    b <- tmp2$IN.FILE %>% 
      str_extract('[^\\.]+') %>% 
      ymd_hms(tz = 'America/Los_Angeles')
    data.acoustic$fileFirst[i] <- min(b)
    data.acoustic$fileLast[i] <- max(b)
    d <- sapply(b, TrapNight)
    data.acoustic$fileTrapNights[i] <- length(unique(d))
  }else{
    if(data.acoustic$filesScrubbed[i] > 0 &
       str_detect(data.acoustic$Path[i],'Processed')){
  a <- list.files(as.character(data.acoustic$Path[i]), recursive = T, pattern = '.wav', ignore.case = T)
  b <- a %>% 
    str_extract('(?<=-)\\d{8}_\\d{6}') %>% 
    ymd_hms(tz = 'America/Los_Angeles')
  data.acoustic$fileFirst[i] <- as.character(min(b))
  data.acoustic$fileLast[i] <- as.character(max(b))
  d <- sapply(b, TrapNight)
  data.acoustic$fileTrapNights[i] <- length(unique(d))
  }else{}
}}



### Which deployments have data outside of deployment time
data.acoustic %>% 
  mutate(tst1 = force_tz(ymd_hms(fileFirst), tzone = 'America/Los_Angeles'),
         tst2 = force_tz(ymd(DeploymentDate) + hours(19) + minutes(30), tzone = 'America/Los_Angeles'),
         early = tst1 - tst2) %>% 
  filter(tst1 < tst2)

data.acoustic %>% 
  mutate(tst1 = force_tz(ymd_hms(fileLast), tzone = 'America/Los_Angeles'),
         tst2 = force_tz(ymd(RecoveryDate) + hours(6) + minutes(30), tzone = 'America/Los_Angeles'),
         late = tst1 - tst2) %>% 
  filter(tst1 > tst2)


### Add Notes for corrections made
data.acoustic <- data.acoustic %>% 
  mutate(Note =
           case_when(LocationNameCONUS == '95426_NW1' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '95426_SW1' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '95901_SE1' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '95901_SW2' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '99612_NE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameCONUS == '99612_NW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameCONUS == '99612_SE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameCONUS == '99612_SW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameCONUS == '96806_NE2' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 8/14/2023 22:31.',
                     LocationNameCONUS == '96806_SE1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 8/14/2023 21:21.',
                     LocationNameCONUS == '96806_SW1' & SiteDeployment == '1' ~ 'Deployment failure, no data provided.',
                     LocationNameCONUS == '97270_NE3' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 7/26/2023 22:40.',
                     LocationNameCONUS == '97270_SE1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 7/27/2023 05:13.',
                     LocationNameCONUS == '97270_SW2' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 7/28/2023 00:37.',
                     LocationNameCONUS == '97729_NW1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 7/24/2023 23:48.',
                     LocationNameCONUS == '97742_SE1' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '99113_NW1' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '99113_SE1' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '99117_SE2' & SiteDeployment == '1' ~ 'Detector clock reset, deployed at 13:00, therefore recorded 13:00 - 19:30 and 08:30 - 08:50, not during standard times. Did not process acoustic data',
                     LocationNameCONUS == '96362_SE2' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '96362_SE3' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '99599_NE1' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '99599_SE1' & SiteDeployment == '1' ~ 'Deployment failure, data provided was duplicate of that belonging to 99599_NE1.',
                     LocationNameCONUS == '99605_SW3' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '99605_SW6' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '99612_SW1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 7/26/2023 23:09. Buzzing in acoustic files.',
                     LocationNameCONUS == '100044_NW1' & SiteDeployment == '1' ~ "Original metadata says deployed 7/10/2023, but data is from 7/18 - 7/20, assuming this is the redeployment Tom had mentioned.",
                     LocationNameCONUS == '100055_NE2' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '100055_NW1' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '100538_NE1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. We don't know when detector was deployed, therefore Survey Start Time, Survey End Time, and File Times are undetermined. Did not process acoustic data. ",
                     LocationNameCONUS == '100538_NW1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. We don't know when detector was deployed, therefore Survey Start Time, Survey End Time, and File Times are undetermined. Did not process acoustic data. ",
                     LocationNameCONUS == '100990_NE2' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '100990_SW2' & SiteDeployment == '1' ~ "Detector clock is 1 year behind, otherwise time correct. Corrected file times. Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '100990_NW2' & SiteDeployment == '1' ~ "Detector clock is set as MDT, local time is PDT, therefore recorded 18:30 - 05:30. Corrected file names.",
                     LocationNameCONUS == '101437_NE1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 7/31/2023 23:01.',
                     LocationNameCONUS == '101437_SE3' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 8/1/2023 01:49.',
                     LocationNameCONUS == '101883_SW1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. We don't know when detector was deployed, therefore Survey Start Time, Survey End Time, and File Times are undetermined. Did not process acoustic data. ",
                     LocationNameCONUS == '101926_NE1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. We don't know when detector was deployed, therefore Survey Start Time, Survey End Time, and File Times are undetermined. Did not process acoustic data. ",
                     LocationNameCONUS == '101926_SW1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. We don't know when detector was deployed, therefore Survey Start Time, Survey End Time, and File Times are undetermined. Did not process acoustic data. ",
                     LocationNameCONUS == '102353_NE1' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '102353_SE2' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '102361_SE1' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '102361_SE3' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '102366_SE4' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '102370_NW3' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '103298_SW2' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '103301_NW1' & SiteDeployment == '1' ~ 'Detector clock appears to be 1 year 1 day behind, otherwise time correct. Corrected file times.',
                     LocationNameCONUS == '103301_SE1' & SiteDeployment == '1' ~ 'Detector clock is 1 year behind, otherwise time correct. Corrected file times.',
                     LocationNameCONUS == '104246_SW1' & SiteDeployment == '1' ~ 'Detector clock reset to 1/1/2000 00:00 at deployment. Deployed at 6/13/2023 14:00, therefore detector recorded 6/13/2023 14:00 - 20:30. Corrected Survey Start/End times and File Times.',
                     LocationNameCONUS == '104648_NE1' & SiteDeployment == '1' ~ 'Deployment failure, no data provided.',
                     LocationNameCONUS == '104648_NW1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. We don't know when detector was deployed, therefore Survey Start Time, Survey End Time, and File Times are undetermined. Did not process acoustic data. ",
                     LocationNameCONUS == '105134_NE1' & SiteDeployment == '1' ~ 'Detector clock reset, deployed at 12:15, therefore recorded 12:15 - 18:45 and 07:45 - 10:45, not during standard times. Did not process acoustic data',
                     LocationNameCONUS == '105159_SE1' & SiteDeployment == '1' ~ 'Detector clock reset to 1/1/2000 00:00 at deployment. Deployed at 6/14/2023 14:45, therefore detector recorded 6/13/2023 14:45 - 21:15. Corrected Survey Start/End times and File Times.',
                     LocationNameCONUS == '105578_NW1' & SiteDeployment == '1' ~ 'Deployment failure, no data provided.',
                     LocationNameCONUS == '105579_NE4' & SiteDeployment == '1' ~ 'Deployment failure, only 1 file recorded, which was classified as Noise.',
                     LocationNameCONUS == '105579_NE5' & SiteDeployment == '1' ~ 'Deployment failure, no data provided.',
                     LocationNameCONUS == '105579_SW1' & SiteDeployment == '1' ~ 'Deployment failure, no data provided.',
                     LocationNameCONUS == '106063_SE2' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '106063_SW2' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '106083_SE3' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '106550_NE4' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '106550_NW2' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '106997_SE1' & SiteDeployment == '1' ~ 'Incomplete survey, last file recorded 6/20/2023 21:40.',
                     LocationNameCONUS == '107000_SW4' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '107000_SW5' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '108393_NE4' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '110232_SW1' & SiteDeployment == '1' ~ 'Detector clock reset, deployed at 12:15, therefore recorded 12:15 - 18:45 and 07:45 - 12:20, not during standard times. Did not process acoustic data',
                     LocationNameCONUS == '110256_NE4' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '110256_SE1' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '110266_SW3' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '110678_NW3' & SiteDeployment == '1' ~ 'Incomplete survey, Out of Memory at 7/12/2023 02:26',
                     LocationNameCONUS == '110698_NE2' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '110698_NE5' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '110711_NW1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. Detector ran ~ 8/14/2023 13:10 - 19:40 and 8/15/2023 08:40 - 10:25. Did not process acoustic data.",
                     LocationNameCONUS == '110715_SE1' & SiteDeployment == '1' ~ 'Detector clock is 1 year behind, otherwise time correct. Corrected File Times.',
                     LocationNameCONUS == '111639_SW1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. Detector ran ~ 8/15/2023 13:50 - 20:20 and 8/16/2023 09:20 - 10:55. Did not process acoustic data.",
                     LocationNameCONUS == '111657_NE1' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '111657_NW3' & SiteDeployment == '1' ~ 'Incomplete survey, Log indicates incomplete run, last file 7/7/2023 20:23',
                     LocationNameCONUS == '112066_SW1' & SiteDeployment == '1' ~ "Detector clock is 1 year behind, otherwise time correct. Corrected file times.",
                     LocationNameCONUS == '112067_NE1' & SiteDeployment == '1' ~ 'Deployment failure, only 3 files recorded, which were all classified as Noise.',
                     LocationNameCONUS == '112565_NW1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. Detector ran ~ 8/16/2023 14:00 - 20:30 and 8/17/2023 09:30 - 10:00. Did not process acoustic data.",
                     LocationNameCONUS == '112582_NW1' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameCONUS == '112077_NW3' & SiteDeployment == '1' ~ 'Incomplete survey, out of memory at 7/25/2023 03:40.',
                     LocationNameCONUS == '113025_NW1' & SiteDeployment == '1' ~ "Detector clock reset to 1/1/2000 00:00 at deployment. Detector ran ~ 8/15/2023 13:40 - 20:10. Did not process acoustic data.",
                     LocationNameCONUS == '113472_NE1' & SiteDeployment == '1' ~ 'Detector clock is 1 year behind, otherwise time correct. Corrected File Times.',
                     LocationNameCONUS == '113478_SW1' & SiteDeployment == '1' ~ 'Incomplete survey, Log has abrupt cutoff, last file 8/2/2023 00:54.',
                     LocationNameCONUS == '113509_SW3' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '114412_NE2' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '115307_SE1' & SiteDeployment == '1' ~ 'Deployment failure, only 6 files recorded during a single minute and a half period.',
                     LocationNameCONUS == '115351_NE1' & SiteDeployment == '1' ~ "Detector D500X 52336 losing all acoustic information from wav files when Attributed with SonoBat. Renamed files using AcousticDataAttribute.R.",
                     LocationNameCONUS == '117633_NE1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 7/28/2023 21:21.',
                     LocationNameCONUS == '117633_SW1' & SiteDeployment == '1' ~ 'Incomplete survey, Out of Memory 7/29/2023 20:41.',
                     LocationNameCONUS == '119480_SW1' & SiteDeployment == '1' ~ 'Deployment failure, only 6 files recorded, which were all classified as Noise.',
                     LocationNameCONUS == '119494_SW1' & SiteDeployment == '1' ~ 'Detector clock is 8 days behind, otherwise time correct. Corrected file times.',
                     LocationNameCONUS == '123197_SW1' & SiteDeployment == '1' ~ 'Deployment failure, no data provided.',
                     LocationNameCONUS == '124138_NE5' & SiteDeployment == '1' ~ 'Deployment failure, no data provided.',
                     LocationNameCONUS == '127360_NE4' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 8/17/2023 04:19.',
                     LocationNameGRTS == '586_NE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '586_SE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '586_SW2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '730_NE1' & SiteDeployment == '1' ~ 'Incomplete survey night of 6/23/2023.',
                     LocationNameGRTS == '730_NW1' & SiteDeployment == '1' ~ 'Incomplete survey night of 7/8/2023.',
                     LocationNameGRTS == '865_NE1' & SiteDeployment == '1' ~ 'Deployment failure, power loss, and only 13 files 6/5/2023 03:51 - 03:58.',
                     LocationNameGRTS == '865_SE1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 6/5/2023 02:34.',
                     LocationNameGRTS == '1841_SW3' & SiteDeployment == '1' ~ "Detector D500X 52334 was recording files with length = 00:00:00, likely because of outdated firmware. Recovered files using D500XWaveFileRestorer program.",
                     LocationNameGRTS == '2378_NE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2378_NE3' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2378_SW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2578_NE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2578_SE3' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2578_SW2' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 6/27/2023 01:51. Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2578_SW3' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2593_NE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2593_SE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2593_SE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2673_NE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2673_NW1' & SiteDeployment == '1' ~ 'Detector clock in PDT and 2 months behind, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2673_SE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '2673_SW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3034_NE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3034_NW2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3034_SW1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 6/13/2023 22:47.',
                     LocationNameGRTS == '3210_NE2' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 7/6/2023 23:01. Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3210_NW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3210_SW2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3361_NE3' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3361_NW1' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3361_SE2' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3361_SW1' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3402_NE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3402_SE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3402_SE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3697_NE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3697_NW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3697_SE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '3697_SW2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4049_NE1' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4049_NW1' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4049_SE1' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4049_SW3' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4129_NE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4129_NW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4129_SE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4138_SE4' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4138_SE5' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4138_SE6' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4138_SW3' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4234_NE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4234_NW2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4234_SE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4234_SW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4273_NE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4273_NW2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4273_SE1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 6/7/2023 05:52. Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4273_SW2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4314_NW2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4314_NW3' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4314_SE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4314_SW2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4385_NE1' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4385_NW1' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4385_NW2' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4385_SW1' & SiteDeployment == '1' ~ 'Detector clock in UTC, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4682_NE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4682_NE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4682_NE3' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4682_SE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4913_NW2' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 6/1/2023 23:03. Detector clock was set 1 year behind by bio, corrected Processed file names.',
                     LocationNameGRTS == '4913_SE1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 6/1/2023 22:07.',
                     LocationNameGRTS == '4913_SE2' & SiteDeployment == '1' ~ 'Incomplete survey, detector clock was 6/1/2023 00:00 at deployment, recorded ~ 14:15 - 20:45 and 09:45 - 11:00, corrected Processed file names. Terrible static in files.',
                     LocationNameGRTS == '4938_NE3' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4938_NW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4938_SE1' & SiteDeployment == '1' ~ 'Incomplete survey, power loss, last file 6/25/2023 05:24. Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '4938_SW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '5153_NE3' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '5153_NW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '5153_SW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '11105_SW1' & SiteDeployment == '1' ~ 'Detector clock was set 3 days behind by bio, corrected Processed file names.',
                     LocationNameGRTS == '11105_SW2' & SiteDeployment == '1' ~ 'Detector clock was set 2 years behind by bio, corrected Processed file names.',
                     LocationNameGRTS == '14273_SE2' & SiteDeployment == '1' ~ 'All files classified as Noise, files appear corrupted and cannot be restored',
                     LocationNameGRTS == '15834_NE1' & SiteDeployment == '1' ~ 'Incomplete survey, microphone fell over during deployment.',
                     LocationNameGRTS == '20817_SW1' & SiteDeployment == '1' ~ 'Incomplete survey, Out of Memory at 6/17/2023 02:55.',
                     LocationNameGRTS == '29914_NW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '29914_SE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '29914_SE3' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '29914_SW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '32074_NW1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '32074_SE1' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '32074_SE2' & SiteDeployment == '1' ~ 'Detector clock in PDT, location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '32074_SW2' & SiteDeployment == '1' ~ 'Detector clock was set 1 year behind by bio (PDT), location is in MDT, corrected Processed file names.',
                     LocationNameGRTS == '125704_NE1' & SiteDeployment == '1' ~ 'Incomplete survey night of 7/11/2023.',
                     LocationNameGRTS == '125704_NE2' & SiteDeployment == '1' ~ 'Incomplete survey night of 7/11/2023.',
                     LocationNameGRTS == '125704_NE3' & SiteDeployment == '1' ~ 'Deployment failure, no acoustic files recorded.',
                     LocationNameGRTS == '125704_NE4' & SiteDeployment == '1' ~ 'Deployment failure, clock error caused detector to activate when deployed, so we do not know actual file times.',
                     TRUE ~ Note))


### Investigate all odd DeploymentNights
data.acoustic %>% 
  distinct(DeploymentNights)

data.acoustic %>% 
  filter(DeploymentNights > 1)
data.acoustic %>% 
  filter(DeploymentNights != 1)
data.acoustic %>% 
  filter(is.na(DeploymentNights))


### Which deployments have data from multiple trap nights
data.acoustic %>% 
  filter(fileTrapNights >1) %>% 
  arrange(fileTrapNights)


### Which deployments don't have acoustic data
data.acoustic %>% 
  filter(fileTrapNights == 0)

data.acoustic %>% 
  filter(is.na(fileTrapNights))

##########################################
### Deployments with no scrubbed files ###
##########################################
data.acoustic %>% 
  filter(filesScrubbed == 0)


######################################################################
### Deployments with mismatched scrubbed and auto-classified files ###
######################################################################
data.acoustic %>% 
  filter(filesScrubbed != filesAutoClassified) %>% 
  group_by(GRTS) %>% 
  count()

######################################
### Acoustic Sites not in Metadata ###
######################################
data.acoustic %>% 
  filter(!(LocationNameCONUS %in% meta$LocationNameCONUS))


######################################
### Metadata Sites not in Acoustic ###
######################################
meta %>% 
  filter(!(LocationNameCONUS %in% data.acoustic$LocationNameCONUS))


#################################################################
### Check that all acoustic files are in SonoBat output files ###
#################################################################
### Read in all CumulativeSonoBatch files
setwd(paste0("E:/NABat/", project, "/Processed/",year))
files <- list.files(recursive = T,pattern = "CumulativeSonoBatch_v420.txt",full.names = T)
SonoBatch <- read_delim(files[1],col_types = cols(.default = "c"))
for(i in 2:length(files)){
  tmp1 <- read_delim(files[i],col_types = cols(.default = "c"))
  SonoBatch <- SonoBatch %>% 
    add_row(tmp1)
  }

### Count number of SonoBatch records for each deployment
data.acoustic <- data.acoustic %>% 
  mutate(filesProcessed = NA_integer_)

for(i in 1:length(data.acoustic$State)){
  tmp1 <- SonoBatch %>% 
    {if(project %in% c("OR","WA"))filter(.,str_detect(ParentDir,data.acoustic$LocationNameCONUS[i])) else
      filter(.,str_detect(ParentDir,data.acoustic$LocationNameGRTS[i]))} %>% 
    {if(data.acoustic$SiteDeployment[i] == 1)filter(.,!str_detect(Path,"\\(\\d\\)")) else
      filter(.,str_detect(Path,paste0("\\(",data.acoustic$SiteDeployment[i],"\\)")))}
  data.acoustic$filesProcessed[i] <- nrow(tmp1)
}

### Identify deployments that don't have the same number of SonoBatch records as Processed files
# Slight differences are to be expected, SonoBatch does not create records for the files it does not append a spp code to
data.acoustic %>% 
  filter(filesProcessed != filesScrubbed) %>% 
  select(State,LocationNameCONUS,LocationNameGRTS,SiteDeployment,filesRecorded,filesAutoClassified,filesProcessed)



###########################
### Save VettingTracker ###
###########################
write_csv(data.acoustic, paste0('E:/NABat/', project, '/VettingTracker_', project, '_', format(Sys.Date(),'%Y%m%d'), '.csv'), na = '')
