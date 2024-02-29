# Install and load packages ----
if(!'pacman' %in% installed.packages()){
  install.packages('pacman')
}

pacman::p_load(tidyverse,
               readxl,
               lubridate,
               readr,
               sf,
               lutz,
               sfheaders)

# Identify yourself, the Project, and year of data to check ----
onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
project <- readline(prompt = "Enter the Project/State corresponding to the acoustic data directory you want to check: {Oregon | Washington | Idaho | USFWS | USFS_Region4}")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")

# List all paths to sites within a directory 
setwd(paste0("E:/NABat/", project, "/Raw/",year))


# Read in CellTracker
CellTracker <- read_excel(paste0("C:/Users/",onid,"/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_",year,".xlsx")) %>% 
  select(CONUS_10KM,GRTS_ID,NABatProject)

# First list all quad level directories
a <- list.dirs(full.names = F)

# Extract path to quad folder
quad.folders <- str_extract(a, '.*\\d+_[NS][EW]\\d[^/]*')
quad.folders <- na.omit(unique(quad.folders))

data.acoustic <- data.frame(Project = project,
                            Path = paste0(getwd(), '/', quad.folders))

# Identify Sample Unit and Station Location of each directory
data.acoustic <- data.acoustic %>% 
  mutate(SU = as.numeric(str_extract(Path, '\\d+(?=_)'))) %>% 
  left_join(CellTracker, by = c('SU' = 'CONUS_10KM')) %>% 
  left_join(CellTracker, by = c('SU' = 'GRTS_ID')) %>% 
  mutate(CONUS =
           case_when(Project %in% c('Oregon','Washington') ~ SU,
                     TRUE ~ CONUS_10KM),
         GRTS = 
           case_when(Project %in% c('Idaho','USFWS','USFS_Region4') ~ SU,
                     TRUE ~ GRTS_ID),
         LocationNameCONUS = paste0(CONUS,str_extract(Path, '_[NS][EW]\\d')),
         LocationNameGRTS = paste0(GRTS,str_extract(Path, '_[NS][EW]\\d'))) %>% 
  select(Project,
         Path,
         CONUS,
         GRTS,
         LocationNameCONUS,
         LocationNameGRTS)

# Ensure all data has valid CONUS and GRTS
data.acoustic %>% 
  filter(is.na(CONUS) |
           is.na(GRTS)) %>% 
  {if(length(.$Project) > 0)(warning(paste0("The following directories have invalid CONUS or GRTS IDs: ",paste(data.acoustic %>% 
                                                                                                                 filter(is.na(CONUS) |
                                                                                                                          is.na(GRTS)) %>% 
                                                                                                                 pull(Path),
                                                                                                               collapse = ", "))))}


### Check to make sure all data is assigned to the correct project
tmp1 <- CellTracker %>% 
  {if(project %in% c('Oregon','Washington')) 
    filter(.,NABatProject == 'NW') 
    else 
      if(project == 'Idaho')
        filter(.,NABatProject == 'ID')
    else
      if(project == 'USFWS')
        filter(.,NABatProject == 'USFWS')
    else
      if(project == 'USFS_Region4')
        filter(.,NABatProject == 'USFS_Region4')
    else .} %>% 
  pull(CONUS_10KM)

if(length(data.acoustic %>% 
          filter(!CONUS %in% tmp1) %>% 
          pull(CONUS)) > 0){
  if(project %in% c('Oregon','Washington')){
  warning(paste0("The following SUs (CONUS) acoustic data are misassigned to ",project," directory: ",paste(data.acoustic %>% 
                                                                                                            filter(!CONUS %in% tmp1) %>% 
                                                                                                            pull(CONUS),
                                                                                                          collapse = ", ")))
  }else{
    warning(paste0("The following SUs (GRTS) acoustic data are misassigned to ",project," directory: ",paste(data.acoustic %>% 
                                                                                                              filter(!CONUS %in% tmp1) %>% 
                                                                                                              pull(GRTS),
                                                                                                            collapse = ", ")))
}}


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

### Set deployment for the obvious ones
data.acoustic <- data.acoustic %>% 
  mutate(SiteDeployment = 
           case_when(str_detect(.$Path, '\\(2\\)') ~ 2,
                     TRUE ~ 1))


###############################################################################################################################
### From metadata we need NewSite, DetectorType, DetectorSN, Deployment.Date, Recovery.Date, NoteAcoustic, and NoteScrubbed ###
###############################################################################################################################
### During the season, before Field Maps editing has been disabled and exported to NABatMetadata{year}_FieldMaps.xlsx, use NABatSurveyForm{year}.csv
if(file.exists(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_FieldMaps.xlsx'))){
  meta1 <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_FieldMaps.xlsx'))
}else{
  meta1 <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatSurveyForm',year,'.csv'))
}

meta1 <- meta1 %>% 
  mutate(LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`),
         LocationNameGRTS = paste0(GRTS, '_', Quad, `Quad Number`),
         DeploymentDate = 
           case_when(is.character(`Deployment Date`) ~ as.Date(mdy_hms(`Deployment Date`), tz = 'America/Los_Angeles'),
                     TRUE ~ as.Date(`Deployment Date`, tz = 'America/Los_Angeles')),
         RecoveryDate = 
           case_when(is.character(`Deployment Date`) ~ as.Date(mdy_hms(`Recovery Date`), tz = 'America/Los_Angeles'),
                     TRUE ~ as.Date(`Recovery Date`, tz = 'America/Los_Angeles')),
         DetectorSN = as.character(`Detector Serial No.`)) %>% 
  select(LocationNameCONUS,
         LocationNameGRTS,
         DeploymentDate,
         RecoveryDate,
         `Detector Type`,
         DetectorSN,
         LocalTime,
         x,
         y) %>% 
  rename(DetectorType = `Detector Type`,
         'Longitude' = 'x',
         'Latitude' = 'y')

### Check to make sure dates calculated correctly, despite the warning
#     "Warning message:
#     There were 2 warnings in `mutate()`.
#     The first warning was:
#        ℹ In argument: `DeploymentDate = case_when(...)`.
#     Caused by warning:
#       ! All formats failed to parse. No formats found."
meta1 %>% 
  distinct(DeploymentDate) %>% 
  arrange(DeploymentDate) %>% 
  print(n=Inf)
meta1 %>% 
  distinct(RecoveryDate) %>% 
  arrange(RecoveryDate) %>% 
  print(n=Inf)

meta2 <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_PaperDatasheets.xlsx'))
meta2 <- meta2 %>% 
  mutate(LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`),
         LocationNameGRTS = paste0(GRTS, '_', Quad, `Quad Number`),
         DeploymentDate = as.Date(`Deployment Date`),
         RecoveryDate = as.Date(`Recovery Date`),
         DetectorSN = as.character(`Detector Serial No.`),
         LocalTime = NA_character_) %>% 
  select(LocationNameCONUS,
         LocationNameGRTS,
         DeploymentDate,
         RecoveryDate,
         `Detector Type`,
         DetectorSN,
         LocalTime,
         Longitude,
         Latitude) %>% 
  rename(DetectorType = `Detector Type`)


meta <- meta1 %>% 
  add_row(meta2)

meta <- meta %>% 
  filter(LocationNameCONUS %in% data.acoustic$LocationNameCONUS)


### Check to see if any Sites have multiple metadata records
meta %>% 
  group_by(LocationNameCONUS) %>% 
  count() %>% 
  filter(n > 1)

### If any Station Locations were surveyed multiple times, add their subsequent deployments by LocationName and DeploymentDate
meta %>% 
  filter(LocationNameCONUS=='100524_NE1')

meta <- meta %>% 
  mutate(SiteDeployment = 
           case_when(LocationNameCONUS == '100524_NE1' &
                       DeploymentDate == '2023-07-06' ~ 2,
                     LocationNameCONUS == '110715_NW1' &
                       DeploymentDate == '2023-07-11' ~ 2,
                     LocationNameCONUS == '110715_SW1' &
                       DeploymentDate == '2023-07-11' ~ 2,
                     LocationNameCONUS == '106997_SE1' &
                       DeploymentDate == '2023-08-22' ~ 2,
                     LocationNameCONUS == '113007_SW1' &
                       DeploymentDate == '2023-08-03' ~ 2,
                     LocationNameCONUS == '131055_SW1' &
                       DeploymentDate == '2023-08-31' ~ 2,
                     TRUE ~ 1))


### Visualize any acoustic data records that don't have corresponding metadata
data.acoustic %>% 
  mutate(tst1 = paste(LocationNameCONUS,SiteDeployment,sep = '_')) %>% 
  filter(!tst1 %in% paste(meta$LocationNameCONUS,meta$SiteDeployment,sep = '_'))


### Join metadata to acoustic
data.acoustic <- data.acoustic %>% 
  left_join(meta)


### Read in existing DetectorCheck file
detector.check <- read_csv(paste0('E:/NABat/', project, '/DetectorCheck_', project, '_',year,'.csv')) %>% 
  mutate(CONUS = as.character(CONUS),
         GRTS = as.character(GRTS))


### Make sure that all records in detector.check occur in data.acoustic
detector.check %>% 
  filter(!(paste0(LocationNameCONUS,SiteDeployment) %in% paste0(data.acoustic$LocationNameCONUS,data.acoustic$SiteDeployment)))

# 100538_NE1, 100538_NW1, 101926_NE1, 101926_SW1 will not be in acoustic.data. They were removed because clock error
# made it impossible to accurately assign time to data. 


### Make data.acoustic contain only New records to add to DetectorCheck
data.acoustic <- data.acoustic %>% 
  anti_join(detector.check, by = c('LocationNameCONUS','SiteDeployment'))

data.acoustic %>% 
  distinct(CONUS,GRTS)

##############################################################################################
### Check to make sure each Path has a single Detector Serial Number that matches metadata ###
##############################################################################################
### What DetectorTypes are present in the data?
data.acoustic %>% 
  distinct(DetectorType)


data.acoustic %>% 
  filter(is.na(DetectorType) | 
           DetectorType=='Other')


#data.acoustic <- data.acoustic %>% 
#  mutate(DetectorType =
#           case_when(LocationNameCONUS == '114412_NE2' &
#                       SiteDeployment == 1 ~ 'D500X',
#                     TRUE ~ DetectorType))


### Add new columns for the Detector, Timezone, and UnusualOccurrences checks
data.acoustic <- data.acoustic %>% 
  mutate(fileDetectorSN = NA_character_,
         MetadataTime = NA_POSIXct_,
         tz_metadata = NA_character_,
         DetectorON = NA_POSIXct_,
         DetectorSleep = NA_POSIXct_,
         tz_detector = NA_character_,
         tz_local = NA_character_,
         Files = NA_integer_,
         FirstFile = NA_character_,
         LastFile = NA_character_,
         TemporalSpread = NA,
         LogNote = NA_character_,
         FullRun = NA)




### Create a function to return the Serial Number from acoustic files for each main detector type
### Attempting to speed up this process by checking serial number of 10 random files when there are > 10 acoustic files
DetectorIdD500x <- function(path){
  setwd(as.character(path))
  tmp1 <- list.files(pattern = '.wav', ignore.case = T)
  if(length(tmp1) > 10){
    dir.create('temp')
    file.copy(sample(list.files(pattern = '.wav', ignore.case = T),10),
              'temp')
    setwd('temp')
    a <- system("powershell -command \"Select-String -Path *.WAV -Pattern '(Serial:\\s+\\d+)|(S/N:\\s+\\d+)' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
    b <- a %>% 
      str_extract('\\d+') %>% 
      unique()
    if(length(b) == 1){
      setwd(as.character(path))
      unlink('temp',recursive = T)
      return(b)
    }else{
      if(length(b) == 0){
        a <- system("powershell -command \"Select-String -Path *.WAV -Pattern 'Serial Number:\\s+\\d+' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
        setwd(as.character(path))
        unlink('temp',recursive = T)
        b <- a %>% 
          str_extract('\\d+') %>% 
          unique()
        if(length(b) == 1){
          return(b)
        }else{
          return('ERROR')
        }
      }
      return('ERROR')
    }
  }else{
  a <- system("powershell -command \"Select-String -Path *.WAV -Pattern '(Serial:\\s+\\d+)|(S/N:\\s+\\d+)' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
  b <- a %>% 
    str_extract('\\d+') %>% 
    unique()
  if(length(b) == 1){
    return(b)
  }else{
    if(length(b) == 0){
      a <- system("powershell -command \"Select-String -Path *.WAV -Pattern 'Serial Number:\\s+\\d+' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
      b <- a %>% 
        str_extract('\\d+') %>% 
        unique()
      if(length(b) == 1){
        return(b)
      }else{
        return('ERROR')
      }
    }
    return('ERROR')
  }
}
}


DetectorIdSM4 <- function(path) {
  setwd(as.character(path))
  tmp1 <- list.files(pattern = '.wav', ignore.case = T)
  if(length(tmp1) > 10){
    dir.create('temp')
    file.copy(sample(list.files(pattern = '.wav', ignore.case = T),10),
              'temp')
    setwd('temp')
    a <- system("powershell -command \"Select-String -Path *.wav -Pattern 'S4U\\d{5}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
    setwd(as.character(path))
    unlink('temp',recursive = T)
    if(length(a) == 1){
      return(a)
    }else{
      return('ERROR')
    }
  }else{
  a <- system("powershell -command \"Select-String -Path *.wav -Pattern 'S4U\\d{5}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
  if(length(a) == 1){
    return(a)
  }else{
    return('ERROR')
  }
}}


DetectorIdSM3 <- function(path) {
  setwd(as.character(path))
  tmp1 <- list.files(pattern = '.wav', ignore.case = T)
  if(length(tmp1) > 10){
    dir.create('temp')
    file.copy(sample(list.files(pattern = '.wav', ignore.case = T),10),
              'temp')
    setwd('temp')
    a <- system("powershell -command \"Select-String -Path *.wav -Pattern 'SM3\\d{5}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
    setwd(as.character(path))
    unlink('temp',recursive = T)
    if(length(a) == 1){
      return(a)
    }else{
      return('ERROR')
    }
  }else{
    a <- system("powershell -command \"Select-String -Path *.wav -Pattern 'SM3\\d{5}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
    if(length(a) == 1){
      return(a)
    }else{
      return('ERROR')
    }
  }}


DetectorIdSMM <- function(path) {
  setwd(as.character(path))
  tmp1 <- list.files(pattern = '.wav', ignore.case = T)
  if(length(tmp1) > 10){
    dir.create('temp')
    file.copy(sample(list.files(pattern = '.wav', ignore.case = T),10),
              'temp')
    setwd('temp')
    a <- system("powershell -command \"Select-String -Path *.wav -Pattern 'SMU\\d{5}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
    setwd(as.character(path))
    unlink('temp',recursive = T)
    if(length(a) == 1){
      return(a)
    }else{
      return('ERROR')
    }
  }else{
    a <- system("powershell -command \"Select-String -Path *.wav -Pattern 'SMU\\d{5}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
    if(length(a) == 1){
      return(a)
    }else{
      return('ERROR')
    }
  }}


DetectorIdSwift <- function(path) {
  setwd(as.character(path))
  tmp1 <- list.files(pattern = '.wav', ignore.case = T)
  if(length(tmp1) > 10){
    dir.create('temp')
    file.copy(sample(list.files(pattern = '.wav', ignore.case = T),10),
              'temp')
    setwd('temp')
    a <- system("powershell -command \"Select-String -Path *.wav -Pattern '(?<=Serial:)\\d+' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
    setwd(as.character(path))
    unlink('temp',recursive = T)
    if(length(a) == 1){
      return(a)
    }else{
      return('ERROR')
    }
  }else{
    if(length(tmp1) != 0){
      a <- system("powershell -command \"Select-String -Path *.wav -Pattern '(?<=Serial:)\\d+' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
      return(a)
    }else{
      return('ERROR')}
  }
}

### Loop through data, finding the DetectorSN from acoustic files for each deployment
### This process is time consuming
for(i in 1:length(data.acoustic$Path)){
  if(data.acoustic$DetectorType[i] == 'D500X' &
     (is.na(data.acoustic$fileDetectorSN[i]) | data.acoustic$fileDetectorSN[i] == 'ERROR')){
    data.acoustic$fileDetectorSN[i] = DetectorIdD500x(data.acoustic$Path[i])
  }else{}
  if(data.acoustic$DetectorType[i] == 'SM4BAT' &
     (is.na(data.acoustic$fileDetectorSN[i]) | data.acoustic$fileDetectorSN[i] == 'ERROR')){
    data.acoustic$fileDetectorSN[i] = DetectorIdSM4(data.acoustic$Path[i])
  }else{}
  if(data.acoustic$DetectorType[i] == 'SM3BAT' &
     (is.na(data.acoustic$fileDetectorSN[i]) | data.acoustic$fileDetectorSN[i] == 'ERROR')){
    data.acoustic$fileDetectorSN[i] = DetectorIdSM3(data.acoustic$Path[i])
  }else{}
  if(data.acoustic$DetectorType[i] %in% c('Song Meter Mini Bat','SMMiniBat') &
     (is.na(data.acoustic$fileDetectorSN[i]) | data.acoustic$fileDetectorSN[i] == 'ERROR')){
    data.acoustic$fileDetectorSN[i] = DetectorIdSMM(data.acoustic$Path[i])
  }else{}
  if(data.acoustic$DetectorType[i] %in% c('AnaSwift','AnaExpress') &
     (is.na(data.acoustic$fileDetectorSN[i]) | data.acoustic$fileDetectorSN[i] == 'ERROR')){
    data.acoustic$fileDetectorSN[i] = DetectorIdSwift(data.acoustic$Path[i])
  }else{}
}


### Loop to count files, pull first and last timestamp, and note Log file errors
# Occasionally datasets will contain an empty last wav file, which will cause an error in this script
# Error in data.acoustic$LastFile[i] <- if (length(files) > 0) { : 
# replacement has length zero
# If this occurs, identify the directory (data.acoustic[i,]) and remove empty wav file.
for(i in 1:length(data.acoustic$Path)){
  files <- list.files(data.acoustic$Path[i], pattern = '.wav', ignore.case = T, recursive = T, full.names = T)
  data.acoustic$Files[i] = length(files)
  files <- list.files(data.acoustic$Path[i], pattern = '.wav', ignore.case = T, full.names = T)
  if(data.acoustic$DetectorType[i] == 'D500X'){
    data.acoustic$FirstFile[i] <- if(length(files) > 0){
      tmp1 <- system(paste0("powershell -command \"Select-String -Path \'", min(files), "\' -Pattern 'File Time:' \""), intern = T)
      if(length(tmp1) == 0){
        tmp1 <- system(paste0("powershell -command \"Select-String -Path \'", min(files), "\' -Pattern 'Timestamp:' \""), intern = T)
        tmp1 <- tmp1[tmp1 != '']
        format(ymd_hms(str_extract(tmp1,'\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}')),'%Y-%m-%d %H:%M:%S')
      }else{
      tmp1 <- tmp1[tmp1 != '']
      format(ymd_hms(str_extract(tmp1,'\\d{6}\\s\\d{2}:\\d{2}:\\d{2}')),'%Y-%m-%d %H:%M:%S')}
    }else{NA}
    data.acoustic$LastFile[i] <- if(length(files) > 0){
      tmp1 <- system(paste0("powershell -command \"Select-String -Path \'", max(files), "\' -Pattern 'File Time:' \""), intern = T)
      if(length(tmp1) == 0){
        tmp1 <- system(paste0("powershell -command \"Select-String -Path \'", max(files), "\' -Pattern 'Timestamp:' \""), intern = T)
        tmp1 <- tmp1[tmp1 != '']
        format(ymd_hms(str_extract(tmp1,'\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}')),'%Y-%m-%d %H:%M:%S')
      }else{
        tmp1 <- tmp1[tmp1 != '']
        format(ymd_hms(str_extract(tmp1,'\\d{6}\\s\\d{2}:\\d{2}:\\d{2}')),'%Y-%m-%d %H:%M:%S')}
    }else{NA}
    if(file.exists(paste0(data.acoustic$Path[i],'/D500X.LOG'))){
      tmp1 <- read_file(paste0(data.acoustic$Path[i],'/D500X.LOG'))
      data.acoustic$DetectorON[i] <- ymd_hms(str_extract(str_extract(tmp1,'.+(?=\\s\\$\\$SYSTEM START)'),'\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}:\\d{2}'))
      data.acoustic$DetectorSleep[i] <- ymd_hms(str_extract(str_extract(tmp1,'.+(?=\\s\\$\\$TIMER SLEEP)'),'\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}:\\d{2}'))
      data.acoustic$LogNote[i] <- if(str_detect(tmp1,'OUT OF MEM|LOW BATTERY')){
        'warning'
      }else{NA}}}
  if(data.acoustic$DetectorType[i] == 'SM4BAT'){
    data.acoustic$FirstFile[i] <- if(length(files) > 0){
      tmp1 <- str_extract(min(files), '\\d{8}_\\d{6}')
      format(ymd_hms(tmp1),'%Y-%m-%d %H:%M:%S')
    }else{NA}
    data.acoustic$LastFile[i] <- if(length(files) > 0){
      tmp1 <- str_extract(max(files), '\\d{8}_\\d{6}')
      format(ymd_hms(tmp1),'%Y-%m-%d %H:%M:%S')
    }else{NA}
  }
  if(data.acoustic$DetectorType[i] == 'SM3BAT'){
    data.acoustic$FirstFile[i] <- if(length(files) > 0){
      tmp1 <- str_extract(min(files), '\\d{8}_\\d{6}')
      format(ymd_hms(tmp1),'%Y-%m-%d %H:%M:%S')
    }else{NA}
    data.acoustic$LastFile[i] <- if(length(files) > 0){
      tmp1 <- str_extract(max(files), '\\d{8}_\\d{6}')
      format(ymd_hms(tmp1),'%Y-%m-%d %H:%M:%S')
    }else{NA}
  }
  if(data.acoustic$DetectorType[i] == 'Song Meter Mini Bat'){
    data.acoustic$FirstFile[i] <- if(length(files) > 0){
      tmp1 <- str_extract(min(files), '\\d{8}_\\d{6}')
      format(ymd_hms(tmp1),'%Y-%m-%d %H:%M:%S')
    }else{NA}
    data.acoustic$LastFile[i] <- if(length(files) > 0){
      tmp1 <- str_extract(max(files), '\\d{8}_\\d{6}')
      format(ymd_hms(tmp1),'%Y-%m-%d %H:%M:%S')
    }else{NA}
  }
}


### Calculate temporal spread between first and last file
data.acoustic <- data.acoustic %>% 
  mutate(TemporalSpread = as.numeric(difftime(ymd_hms(LastFile), ymd_hms(FirstFile), units = 'mins')))


### Identify Log files that cutoff, potentially indicating incomplete survey
for(i in 1:length(data.acoustic$Path)){
  if(data.acoustic$DetectorType[i] == 'D500X' &
     file.exists(paste0(data.acoustic$Path[i],'/D500X.LOG'))){
    tmp1 <- read_file(paste0(data.acoustic$Path[i],'/D500X.LOG'))
    while(str_detect(tmp1,'TIMER WAKEUP')){
      tmp1 <- str_extract(tmp1,'(?<=TIMER WAKEUP)[\\s\\S]+')}
    data.acoustic$FullRun[i] <- str_detect(tmp1,'TIMER SLEEP')
  }
}


### Identify deployments with GUANO metadata, which may cause problems in SonoBat
### Scrub normally, then rename with AcousticDataAttribute.R prior to auto-classifying
guano <- NULL
for(i in 1:length(data.acoustic$Path)){
  if(data.acoustic$DetectorType[i] == 'D500X'){
    files <- list.files(data.acoustic$Path[i],pattern = '.WAV')
    if(length(files) > 0){
    tmp1 <- paste0(data.acoustic$Path[i],"/",files[1])
    a <- system(paste0("powershell -command \"Select-String -Path ",tmp1," -Pattern 'GUANO'\""), intern = T)
    if(length(a)>0){
      if(ST %in% c('OR','WA')){
        guano <- c(guano,data.acoustic$LocationNameCONUS[i])}else{
          guano <- c(guano,data.acoustic$LocationNameGRTS[i])}}
    }}}
guano


### Identify deployments with D500X Firmware 2.4.4, which may cause problems in SonoBat
### Run through D500XWaveFileRestorer prior to SonoBat processing
firm <- NULL
for(i in 1:length(data.acoustic$Path)){
  if(data.acoustic$DetectorType[i] == 'D500X'){
    files <- list.files(data.acoustic$Path[i],pattern = '.WAV')
    if(length(files) > 0){
      tmp1 <- paste0(data.acoustic$Path[i],"/",files[1])
      a <- system(paste0("powershell -command \"Select-String -Path ",tmp1," -Pattern '2\\.4\\.4'\""), intern = T)
      if(length(a)>0){
        if(ST %in% c('OR','WA')){
          firm <- c(firm,data.acoustic$LocationNameCONUS[i])}else{
            firm <- c(firm,data.acoustic$LocationNameGRTS[i])}}
    }}}
firm


###################################
### Checks about Detector Clock ###
###################################
### This is primarily of interest for OSU deployments in Idaho, where we have not historically had crews correct detector time.
### It is also useful for checking the timezone of SM4 files
### Add timezone for each location
data.acoustic <- data.acoustic %>% 
  st_as_sf(coords = c('Longitude','Latitude'),
           crs = 4269) %>% 
  mutate(tz_local = tz_lookup(.,method = 'accurate')) %>% 
  sf_to_df(fill = T) %>% 
  select(-c(sfg_id,point_id,x,y))


### Add timezone of device used to collect metadata
data.acoustic <- data.acoustic %>% 
  mutate(tz_metadata =
           case_when(str_extract(LocalTime,"-\\d{2}:\\d{2}$") == "-07:00" ~ 'America/Los_Angeles',
                     str_extract(LocalTime,"-\\d{2}:\\d{2}$") == "-06:00" ~ 'America/Boise',
                     TRUE ~ NA_character_))


### Add local time, in device timezone
data.acoustic <- data.acoustic %>% 
  mutate(MetadataTime = ymd_hms(str_extract(data.acoustic$LocalTime,"\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}"))) %>% 
  select(-LocalTime)


### Assign timezone of detector, if possible, for D500x and SM4 detectors
data.acoustic <- data.acoustic %>% 
  mutate(tz_detector =
           case_when((tz_metadata == 'America/Los_Angeles' &
                        abs(DetectorSleep - MetadataTime) <= minutes(15)) |
                       (tz_metadata == 'America/Boise' &
                          MetadataTime - DetectorSleep >= minutes(45) &
                          MetadataTime - DetectorSleep <= minutes(75)) ~ "America/Los_Angeles",
                     (tz_metadata == 'America/Boise' &
                        abs(DetectorSleep - MetadataTime) <= minutes(15)) |
                       (tz_metadata == 'America/Los_Angeles' &
                          DetectorSleep - MetadataTime >= minutes(45) &
                          DetectorSleep - MetadataTime <= minutes(75)) ~ "America/Boise",
                     LocationNameGRTS == "865_NE1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "1633_NE3" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "1745_SW1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "2593_SE1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "2673_NW1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "4129_NW1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "4273_NW2" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "4913_NW2" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "4913_SE2" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "4938_SW1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "5985_NW1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "5985_SE1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "11105_SW1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "11105_SW2" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "14673_SW3" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "20817_SW1" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     LocationNameGRTS == "32074_SW2" & SiteDeployment == 1 ~ "America/Los_Angeles",
                     TRUE ~ "check"))

### Add timezone for SM4 detectors
for(i in 1:length(data.acoustic$Project)){
  if(data.acoustic$DetectorType[i] == 'SM4BAT'){
    setwd(data.acoustic$Path[i])
    tmp1 <- list.files(pattern = '.wav', ignore.case = T)
    if(length(tmp1) > 10){
      dir.create('temp')
      file.copy(sample(list.files(pattern = '.wav', ignore.case = T),10),
                'temp')
      setwd('temp')
      a <- system("powershell -command \"Select-String -Path *.wav -Pattern '(?<=\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})-\\d{2}:\\d{2}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
      setwd(data.acoustic$Path[i])
      unlink('temp',recursive = T)
      if(length(a) == 1){
        data.acoustic$tz_detector[i] <- a
      }else{
        data.acoustic$tz_detector[i] <- 'ERROR'
      }
    }else{
      a <- system("powershell -command \"Select-String -Path *.wav -Pattern '(?<=\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})-\\d{2}:\\d{2}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
      if(length(a) == 1){
        data.acoustic$tz_detector[i] <- a
      }else{
        data.acoustic$tz_detector[i] <- 'ERROR'
      }
    }
  }
}

data.acoustic %>% 
  filter(DetectorType == 'SM4BAT') %>% 
  distinct(tz_detector)

### Add timestamp for Anabat Swift detectors
for(i in 1:length(data.acoustic$Project)){
  if(data.acoustic$DetectorType[i] == 'AnaSwift'){
    setwd(data.acoustic$Path[i])
    tmp1 <- list.files(pattern = '.wav', ignore.case = T)
    if(length(tmp1) > 10){
      dir.create('temp')
      file.copy(sample(list.files(pattern = '.wav', ignore.case = T),10),
                'temp')
      setwd('temp')
      a <- system("powershell -command \"Select-String -Path *.wav -Pattern '(?<=\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+)-\\d{2}:\\d{2}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
      setwd(data.acoustic$Path[i])
      unlink('temp',recursive = T)
      if(length(a) == 1){
        data.acoustic$tz_detector[i] <- a
      }else{
        data.acoustic$tz_detector[i] <- 'ERROR'
      }
    }else{
      a <- system("powershell -command \"Select-String -Path *.wav -Pattern '(?<=\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+)-\\d{2}:\\d{2}' | foreach {$_.matches.value} | Get-Unique\"", intern = T)
      if(length(a) == 1){
        data.acoustic$tz_detector[i] <- a
      }else{
        data.acoustic$tz_detector[i] <- 'ERROR'
      }
    }
  }
}

data.acoustic %>% 
  filter(DetectorType == 'AnaSwift') %>% 
  distinct(tz_detector)

data.acoustic <- data.acoustic %>% 
  mutate(tz_detector =
           case_when(tz_detector == "-00:00" ~ "GMT",
                     tz_detector == "-06:00" ~ "America/Boise",
                     tz_detector == "-07:00" ~ "America/Los_Angeles",
                     TRUE ~ tz_detector))


### Check out deployments without obvious detector time zone, add individually identified time zones above
data.acoustic %>% 
  filter(tz_detector == "check") %>% 
  select(LocationNameCONUS,LocationNameGRTS,SiteDeployment,DetectorType,DetectorSN,tz_metadata,MetadataTime,DetectorSleep)
# 3729_SE2 No usable Log file
# 100487_NE4 No usable log file

### Investigate deployments where the biologist changed the clock
data.acoustic %>% 
  filter(DetectorType == "D500X" &
           abs(DetectorON - DetectorSleep) > minutes(15)) %>% 
  select(Project,LocationNameCONUS,LocationNameGRTS,DetectorSN,MetadataTime,tz_metadata,DetectorON,DetectorSleep) %>% 
  arrange(desc(abs(DetectorON - DetectorSleep)))


### Investigate deployments where the detector sleep time is not close to local time
#data.acoustic %>% 
#  filter(DetectorType == "D500X" &
#           abs(MetadataTime - DetectorSleep) > minutes(15)) %>% 
#  mutate(tz_check =
#           case_when(tz_local == tz_metadata ~ T,
#                     TRUE ~ F)) %>% 
#  select(LocationNameCONUS,LocationNameGRTS,DetectorSN,MetadataTime,DetectorSleep,tz_check,tz_metadata) %>% 
#  arrange(desc(abs(MetadataTime - DetectorSleep)))



### Deployments where detector not recording in local time
# Make corrections in metadata Recording Start/Stop fields
# Correct acoustic file name timestamps once attributed
data.acoustic %>% 
  filter(tz_local != tz_detector) %>% 
  select(LocationNameCONUS,LocationNameGRTS,SiteDeployment,DetectorType,DetectorSN,tz_local,tz_detector)

### Deployments in MDT
data.acoustic %>% 
  filter(tz_detector == "America/Boise") %>% 
  select(LocationNameGRTS,SiteDeployment,DetectorSN,tz_metadata,tz_local,tz_detector)

###################################
###################################
###################################

### Investigate acoustic data where no fileDetectorSN assigned
data.acoustic %>% 
  filter(is.na(fileDetectorSN))


### Investigate deployments with fileDetectorSN errors
data.acoustic %>% 
  filter(fileDetectorSN == 'ERROR')
### 104648_NE1 AllNoise


### Investigate deployments with non-matching SN
data.acoustic %>% 
  filter(!is.na(.$fileDetectorSN)) %>% 
  mutate(SNtest = str_detect(.$fileDetectorSN, as.character(.$DetectorSN))) %>% 
  filter(SNtest == FALSE & fileDetectorSN != 'ERROR')


### Investigate deployments with few files
data.acoustic %>% 
  filter(Files < 25 &
           (TemporalSpread >= 500 |
              is.na(TemporalSpread)) &
           is.na(LogNote) &
           (FullRun == T |
              is.na(FullRun))) %>% 
  select(LocationNameCONUS,LocationNameGRTS,SiteDeployment,DeploymentDate,DetectorSN,Files,FirstFile,LastFile,TemporalSpread) %>% 
  arrange(Files)


### Investigate deployments with short temporal spread between files
data.acoustic %>% 
  filter(TemporalSpread < 500 &
           is.na(LogNote) &
           (FullRun == T |
              is.na(FullRun))) %>% 
  select(LocationNameCONUS,LocationNameGRTS,SiteDeployment,DeploymentDate,DetectorSN,Files,FirstFile,LastFile,TemporalSpread) %>% 
  arrange(TemporalSpread)


### Investigate deployments with Log file warnings
data.acoustic %>% 
  filter(LogNote == 'warning') %>% 
  select(LocationNameCONUS,LocationNameGRTS,SiteDeployment,DeploymentDate,DetectorSN,Files,FirstFile,LastFile,TemporalSpread)


### Investigate Log files that are cut-off or missing
data.acoustic %>% 
  filter(FullRun == F &
           is.na(LogNote)) %>% 
  select(LocationNameCONUS,LocationNameGRTS,SiteDeployment,DeploymentDate,DetectorSN,Files,FirstFile,LastFile,TemporalSpread)


### Look for early and late files
data.acoustic %>% 
  mutate(tst1 = force_tz(ymd_hms(FirstFile), tzone = 'America/Los_Angeles'),
         tst2 = force_tz(ymd(DeploymentDate) + hours(19) + minutes(30), tzone = 'America/Los_Angeles'),
         early = tst1 - tst2) %>% 
  filter(tst1 < tst2)

data.acoustic %>% 
  mutate(tst1 = force_tz(ymd_hms(LastFile), tzone = 'America/Los_Angeles'),
         tst2 = force_tz(ymd(RecoveryDate) + hours(6) + minutes(30), tzone = 'America/Los_Angeles'),
         late = tst1 - tst2) %>% 
  filter(tst1 > tst2)




head(detector.check)
head(data.acoustic)

str(detector.check)
str(data.acoustic)

detector.check <- detector.check %>% 
  mutate(FirstFile = as.character(FirstFile),
         LastFile = as.character(LastFile),
         DetectorSN = as.character(DetectorSN),
         fileDetectorSN = as.character(fileDetectorSN),
         CONUS = as.numeric(CONUS),
         GRTS = as.numeric(GRTS))

names(data.acoustic)[!names(data.acoustic)%in%names(detector.check)]
#detector.check <- detector.check %>% 
#  mutate(MetadataTime = NA_POSIXct_,
#         tz_metadata = NA_character_,
#         DetectorON = NA_POSIXct_,
#         DetectorSleep = NA_POSIXct_,
#         tz_detector = NA_character_,
#         tz_local = NA_character_)

### Add new records to existing file
data.acoustic <- detector.check %>% 
  add_row(data.acoustic)


### Check to see how many surveys per cell
### Do any cell folders need to have _Incomplete added to name?
data.acoustic %>% 
  group_by(CONUS,GRTS) %>% 
  count() %>% 
  filter(n < 4) %>% 
  arrange(n)

write_csv(data.acoustic, paste0('E:/NABat/', project, '/DetectorCheck_', project, '_',year,'.csv'))



############################################
### Consider Adding Firmware calculation ###
############################################
### Add a new column for the Detector Firmware derrived from acoustic files
data.acoustic <- data.acoustic %>% 
  mutate(fileDetectorFirmware = NA_character_)

### Firmware functions
FirmwareD500x <- function(path){
  setwd(as.character(path))
  if(length(list.files()) == 0){return('NoFiles')}else{
    tmp1 <- sample(list.files(),1)
    tmp2 <- system(paste0('powershell -command \"Select-String -Path \'',tmp1,'\' -Pattern \'Firmware Version:.+\' | foreach {$_.matches.value}\"'),intern = T)
    tmp3 <- str_extract(tmp2,'(?<=D500X\\sV).{5}')
    return(tmp3)}}

FirmwareSM4 <- function(path){
  setwd(as.character(path))
  if(length(list.files()) == 0){return('NoFiles')}else{
    tmp1 <- sample(list.files(),1)
    tmp2 <- system(paste0('powershell -command \"Select-String -Path \'',tmp1,'\' -Pattern \'Firmware Version:.+\' | foreach {$_.matches.value}\"'),intern = T)
    tmp3 <- str_extract(tmp2,'(?<=Version:).+')
    tmp3 <- str_extract(tmp3,'[^\\s]+')
    return(tmp3)}}

FirmwareSwift <- function(path){
  setwd(as.character(path))
  if(length(list.files()) == 0){return('NoFiles')}else{
    tmp1 <- sample(list.files(),1)
    tmp2 <- system(paste0('powershell -command \"Select-String -Path \'',tmp1,'\' -Pattern \'Firmware Version:.+\' | foreach {$_.matches.value}\"'),intern = T)
    tmp3 <- str_extract(tmp2,'(?<=\\s)\\d\\.\\d')
    return(tmp3)}}

FirmwareExpress <- function(path){
  setwd(as.character(path))
  setwd('..')
  setwd(str_replace(getwd(),'Raw','Processed'))
  tmp1 <- read.csv('meta.csv',colClasses = c('character'))
  tmp2 <- tmp1 %>% 
    filter(str_detect(FOLDER,data.acoustic$Site[i])) %>% 
    distinct(FIRMWARE) %>% 
    pull(FIRMWARE)
  if(length(tmp2) == 1){return(tmp2)}else{
    return(NA_character_)
  }
}

for(i in 1:length(data.acoustic$Path)){
  if(is.na(data.acoustic$fileDetectorFirmware[i]) & data.acoustic$DetectorType[i] == 'D500X'){
    data.acoustic$fileDetectorFirmware[i] = FirmwareD500x(data.acoustic$Path[i])}
  if(is.na(data.acoustic$fileDetectorFirmware[i]) & data.acoustic$DetectorType[i] == 'SM4BAT'){
    data.acoustic$fileDetectorFirmware[i] = FirmwareSM4(data.acoustic$Path[i])}
  if(is.na(data.acoustic$fileDetectorFirmware[i]) & data.acoustic$DetectorType[i] == 'SM3BAT'){
    data.acoustic$fileDetectorFirmware[i] = FirmwareSM4(data.acoustic$Path[i])}
  if(data.acoustic$DetectorType[i] == 'SMMiniBat'){
    data.acoustic$fileDetectorFirmware[i] = NA_character_}
  if(is.na(data.acoustic$fileDetectorFirmware[i]) & data.acoustic$DetectorType[i] == 'Anabat Swift'){
    data.acoustic$fileDetectorFirmware[i] = FirmwareSwift(data.acoustic$Path[i])}
  if(is.na(data.acoustic$fileDetectorFirmware[i]) & data.acoustic$DetectorType[i] == 'Anabat Express'){
    data.acoustic$fileDetectorFirmware[i] = FirmwareExpress(data.acoustic$Path[i])}
}
