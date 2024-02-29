### This script is used to compile vetted output files into a single file by Project
### To ensure that R is able to read all vetted output files, navigate to the VettedOutput directory(s) in Box Drive in File Explorer and refresh

library(tidyverse)
library(readxl)


onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
project <- readline(prompt = "Enter the NABat Project corresponding to the acoustic data directory you want to compile: {NW | ID | USFWS | USFS_Region04}")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")


# SonoBat Output ----
## Read in all Database.txt files from the project of interest and combine ----
# Identify the direstories to pull Vetted Output .txt files from
if(project == 'NW'){
  folders <- c(paste0('C:/Users/',onid,'/Box/HERS_BatAcousticFiles/NABat/Oregon/Processed/',year,'/OR_VettedOutput'),
               paste0('C:/Users/',onid,'/Box/HERS_BatAcousticFiles/NABat/Washington/Processed/',year,'/WA_VettedOutput'))
}else{
  if(project == 'ID'){
    folders <- paste0('C:/Users/',onid,'/Box/HERS_BatAcousticFiles/NABat/Idaho/Processed/',year,'/Idaho_VettedOutput')
  }else{
    if(project == 'USFS_Region04'){
      folders <- paste0('C:/Users/',onid,'/Box/HERS_BatAcousticFiles/NABat/USFS_Region04/Processed/',year,'/R04VettedOutput')
    }else{if(project == 'USFWS'){
      folders <- paste0('C:/Users/',onid,'/Box/HERS_BatAcousticFiles/NABat/USFWS/Processed/',year,'/USFWSVettedOutput')
    }else{
      if(!project %in% c('NW','ID','USFWS','USFS_Region04')){
        folders <- NULL
        warning('The Project you entered is incompatible with this script. Please enter a valid Project, or edit this script as needed.')
      }}}}}
folders

# Read in all files from the identified directories and combine
acoustic <- NULL
for(i in 1:length(folders)){
  setwd(folders[i])
  files <- list.files(full.names = T)
  tmp1 <- read_tsv(files[1], col_types = cols(.default = "c"))
  for(j in 2:length(files)){
    tmp2 <- read_tsv(files[j], col_types = cols(.default = "c"))
    tmp1 <- tmp1 %>% 
      add_row(tmp2)
    }
  acoustic <- rbind(acoustic,tmp1)
  }

## Format data ----
# Read in Cell List for CONUS and GRTS calculation
CellList <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx'), col_types = c('text')) %>% 
  select(1:2)

# Calculate GRTS and CONUS for each file
acoustic <- acoustic %>% 
  mutate(Cell = str_extract(ParentDir, '\\d+')) %>% 
  left_join(CellList, by = c('Cell' = 'CONUS_10KM')) %>% 
  left_join(CellList, by = c('Cell' = 'GRTS_ID')) %>% 
  mutate(CONUS =
           case_when(project == 'NW' ~ Cell,
                     TRUE ~ CONUS_10KM),
         GRTS =
           case_when(project == 'NW' ~ GRTS_ID,
                     TRUE ~ Cell),
         LocationNameCONUS = paste0(CONUS,str_extract(ParentDir,'_[NS][EW]\\d')),
         LocationNameGRTS = paste0(GRTS,str_extract(ParentDir,'_[NS][EW]\\d'))) %>% 
  select(-c(Cell,GRTS_ID,CONUS_10KM))
  
# Check that there are no invalid GRTS values
acoustic %>% 
  filter(is.na(GRTS)|
           GRTS == '') 

# Check that there are no invalid Site values
acoustic %>% 
  filter(is.na(LocationNameCONUS)|
           LocationNameCONUS == '') %>% 
  distinct(ParentDir)

# Correct Site for those with non-standard names
#acoustic <- acoustic %>% 
#  mutate(Site =
#           case_when(ParentDir == '117633_FlightsEnd1' ~ '117633_NW4',
#                     ParentDir == '117633_FlightsEnd2' ~ '117633_NW6',
#                     ParentDir == '117633_FlightsEnd5' ~ '117633_NW7',
#                     ParentDir == '117633_FlightsEnd6' ~ '117633_NW8',
#                     ParentDir == '96350_Unk1' ~ '96350_Unk1',
#                     ParentDir == '96350_Unk2' ~ '96350_Unk2',
#                     TRUE ~ Site))


## Correct MonitoringNight ----
# Check to make sure MonitoringNight matches file time
# Corrections made to Processed acoustic file names and SonoBatch outputs do not carry over to SonoVet output
# MonitoringNight is calculated from internal file timestamp, which is never edited in our corrections. 
acoustic %>% 
  mutate(tmp1 = as.character(as.Date(ymd_hms(str_extract(Filename,'\\d{8}_\\d{6}')) - lubridate::hours(12)))) %>% 
  filter(MonitoringNight != tmp1) %>% 
  distinct(LocationNameCONUS,LocationNameGRTS,MonitoringNight,tmp1)

# Notes about differences between internal clock and timestamp from file name
# Check for notes in Box\Raw\YYYY\Cell folder
# 2673_NW1, clock set 2 months behind
# 4049_SW3 timestamp is in UTC, subtracted 6 hours for MDT
# 100990_SW2, clock set one year behind
# 103301_NW1, clock set one year and one day behind
# 103301_SE1, clock set one year behind
# 104219_NE1, original acoustic files 8/15 - 8/16 corrected to 8/10 - 8/11
# 104246_SW2, clock reset to 1/1/2000. Corrected processed files 
# 105159_SE1, clock reset to 1/1/2000. Corrected processed files
# 106536_NE1, original acoustic files 8/21 - 8/22 corrected to 8/24 - 8/25
# 123184_SE1, clock was incorrectly set a month behind
# 123688_NW2, clock was incorrectly set a month behind
# 125970_NW2, clock was incorrectly set a month behind
# 125985_SE3, clock was incorrectly set a month behind
# 125993_NE3, clock was incorrectly set a month behind
# 127847_SE7, clock was incorrectly set a month behind
# 128752_NW1, clock was incorrectly set a month behind
# 130146_SW3, clock was incorrectly set a month behind
# 130151_SW1, clock was incorrectly set a month behind
# 93572_NW1, clock reset at deployment

# Calculate MonitoringNight from timestamp in Filename
acoustic <- acoustic %>% 
  mutate(tmp1 = as.character(as.Date(ymd_hms(str_extract(Filename,'\\d{8}_\\d{6}')) - lubridate::hours(12))),
         MonitoringNight =
           case_when(MonitoringNight != tmp1 ~ tmp1,
                     TRUE ~ MonitoringNight)) %>% 
  select(-tmp1)

# Check for MonitoringNight from wrong year
acoustic %>% 
  distinct(MonitoringNight) %>% 
  arrange(MonitoringNight) %>% 
  print(n=Inf)

## Add Site Deployment ----
# Use metadata to identify Station Locations surveyed multiple times
meta1 <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatSurveyForm',year,'.csv'))
meta1 <- meta1 %>% 
  mutate(LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`),
         DeploymentDate = as.Date(mdy_hms(`Deployment Date`), tz = 'America/Los_Angeles')) %>% 
  select(LocationNameCONUS,
         DeploymentDate)

meta2 <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABatMetadata',year,'_PaperDatasheets.xlsx'))
meta2 <- meta2 %>% 
  mutate(LocationNameCONUS = paste0(CONUS, '_', Quad, `Quad Number`),
         DeploymentDate = as.Date(`Deployment Date`)) %>% 
  select(LocationNameCONUS,
         DeploymentDate)

meta <- meta1 %>% 
  add_row(meta2) %>% 
  filter(!str_detect(LocationNameCONUS,'116234|116697'))     # Remove ODFW_Tillamook_SandLake locations

# Identify Station Locations with multiple metadata records
meta %>% 
  filter(LocationNameCONUS %in% (meta %>% 
                                   group_by(LocationNameCONUS) %>% 
                                   count() %>% 
                                   filter(n > 1) %>% 
                                   pull(LocationNameCONUS))) %>% 
  arrange(LocationNameCONUS,DeploymentDate) %>% 
  print(n=Inf)

# Identify all Monitoring Nights associated with each resurveyed Station Location
acoustic %>% 
  filter(LocationNameCONUS %in% c('100524_NE1','106997_SE1','110715_NW1','110715_SW1','113007_SW1','131055_SW1')) %>% 
  distinct(LocationNameCONUS,LocationNameGRTS,MonitoringNight) %>% 
  arrange(LocationNameCONUS,MonitoringNight)

# Assign correct SiteDeployment to all Monitoring Nights
acoustic <- acoustic %>% 
  mutate(SiteDeployment = 
           case_when(LocationNameCONUS == '100524_NE1' &
                       MonitoringNight == '2023-07-06' ~ 2,
                     LocationNameCONUS == '106997_SE1' &
                       MonitoringNight == '2023-08-22' ~ 2,
                     LocationNameCONUS == '110715_NW1' &
                       MonitoringNight %in% c('2023-07-11','2023-07-12') ~ 2,
                     LocationNameCONUS == '110715_SW1' &
                       MonitoringNight %in% c('2023-07-11','2023-07-12') ~ 2,
                     LocationNameCONUS == '113007_SW1' &
                       MonitoringNight == '2023-08-03' ~ 2,
                     LocationNameCONUS == '131055_SW1' &
                       MonitoringNight == '2023-08-31' ~ 2,
                     TRUE ~ 1)) %>% 
  select(CONUS,GRTS,LocationNameCONUS,LocationNameGRTS,SiteDeployment,1:38)


## Save combined acoustic data ----
write_csv(acoustic,paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/AcousticOutput_NABat_',project,'_SonoBat_',year,'.csv'))



# Zero-crossing ----
setwd(paste0('C:/Users/',onid,'/Box/HERS_BatAcousticFiles/NABat/Idaho/Processed/',year,'/KaleidoscopeResults'))
files <- list.files()
files
acoustic.zc <- NULL

for(i in 1:length(files)){
  tmp1 <- read.csv(files[i], colClasses = c('character'))
  tmp1 <- tmp1 %>% 
    filter(AUTO.ID. != 'Noise')
  acoustic.zc <- rbind(acoustic.zc, tmp1)}

acoustic.zc <- acoustic.zc %>% 
  mutate(State = 'ID',
         Site = str_extract(FOLDER, '\\d+_[NS][EW]\\d'),
         GRTS = str_extract(FOLDER, '\\d+')) %>% 
  left_join(CellList, by = c('GRTS' = 'GRTS_ID')) %>% 
  #rename('CONUS' = 'CONUS_10KM')


### Add MonitoringNight and Site Deployment
acoustic.zc <- acoustic.zc %>% 
  mutate(MonitoringNight = mdy(DATE.12),
         SiteDeployment = 
           case_when(Site == '1569_NE2' &
                       MonitoringNight == '2023-08-07' ~ 2,
                     Site == '1569_NW1' &
                       MonitoringNight == '2023-08-07' ~ 2,
                     Site == '1569_SE1' &
                       MonitoringNight == '2023-08-08' ~ 2,
                     Site == '105579_SE1' &
                       MonitoringNight == '2022-07-25' ~ 2,
                     Site == '110673_NW2' &
                       MonitoringNight == '2022-08-15' ~ 2,
                     Site == '111606_SW3' &
                       MonitoringNight == '2022-06-14' ~ 2,
                     Site == '113478_NE1' &
                       MonitoringNight == '2022-07-21' ~ 2,
                     Site == '97272_NE1' &
                       MonitoringNight == '2022-08-22' ~ 2,
                     Site == '394_SE1' &
                       MonitoringNight %in% as.Date(c('2022-08-29','2022-08-30','2022-08-31')) ~ 2,
                     Site == '394_SW1' &
                       MonitoringNight %in% as.Date(c('2022-08-29','2022-08-30','2022-08-31')) ~ 2,
                     TRUE ~ 1)) %>% 
  select(45,47,48,46,50,49,1:44)

names(acoustic.zc)


### Separate files by project, if applicable


### Save combined acoustic data
write_csv(acoustic.zc,paste0('C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/AcousticOutput_NABat_Kaleidoscope_2023.csv', na = ''))
