### tblDeploymentSoftware ###

### Relationships ###
### tblDeploymentSoftware::DeploymentID <---> tblDeployment::ID
### tblDeploymentSoftware::SoftwareID <---> tluSoftware::ID
### tblDeploymentSoftware::ContactID <---> tluContact_1::ID

library(tidyverse)
library(sf)
library(DBI)
library(readxl)
library(lubridate)

con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/emblidgp/Desktop/PNW_BatHub_Database.accdb")

## DeploymentID ####
### Deployments missing related tblDeploymentSoftware records
tblDeployment <- dbReadTable(con,'tblDeployment')
tblDeploymentSoftware <- dbReadTable(con,'tblDeploymentSoftware')
tblPointLocation <- dbReadTable(con,'tblPointLocation')

dat <- tblDeployment %>% 
  select('DeploymentID' = ID,PointLocationID,DeploymentDate) %>% 
  left_join(tblPointLocation %>% 
              select(ID,LocationName,LocationName_GRTS),
            by = c("PointLocationID" = "ID")) %>% 
  left_join(tblDeploymentSoftware %>% 
              select(DeploymentID,SoftwareID),
            by = c('DeploymentID')) %>% 
  filter(is.na(SoftwareID))

dat %>% 
  distinct(DeploymentDate) %>% 
  arrange(DeploymentDate)

dat %>% 
  dim()



## SoftwareID ####
# Identify zero-crossing data processed with Kaleidoscope
dat_Kaleidoscope <- read_csv("C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/AcousticOutput_NABat_PNW_Kaleidoscope_2022.csv")
dat_Kaleidoscope <- dat_Kaleidoscope %>% 
  mutate(LocationName = paste0(CONUS,'_',str_extract(Site,'[NS][EW]\\d')))

# Identify any Station Locations from ZC SUs that do not occur in dat_Kaleidoscope
tmp1 <- dat %>% 
  filter(str_detect(DeploymentDate,'2022') &
           str_extract(LocationName,'^\\d+') %in% dat_Kaleidoscope$CONUS &
           !LocationName %in% dat_Kaleidoscope$LocationName) %>% 
  pull(LocationName)

# Add a record to dat_Kaleidoscope for ZC deployments with no acoustic data
dat_Kaleidoscope <- dat_Kaleidoscope %>% 
  add_row(data.frame(LocationName = tmp1))

# Assign SoftwareID
dat <- dat %>% 
  filter(str_detect(DeploymentDate,'2022')) %>% 
  mutate(SoftwareID =
           case_when(LocationName %in% dat_Kaleidoscope$LocationName ~ '1',
                     TRUE ~ '7')) 


## ContactID ####
tblDeploymentSoftware %>% 
  group_by(ContactID) %>% 
  count()

tluContact <- dbReadTable(con,'tluContact')
tluContact %>% 
  filter(ID %in% c(83,55,138,139,214))

### We'll credit all 2022 data processing to Patrick
tluContact %>% 
  filter(LastName=='Emblidge')
dat <- dat %>% 
  mutate(ContactID = 214)


## Version ####
tblDeploymentSoftware %>% 
  group_by(Version) %>% 
  count()

dat <- dat %>% 
  mutate(Version =
           case_when(SoftwareID == 7 ~ '4.2',
                     SoftwareID == 1 ~ '5.4.8'))

# 2020 Kaleidoscope data processed with 5.4.0
# 2021 Kaleidoscope data processed with 5.4.2
# 2022 Kaleidoscope data processed with 5.4.8


## ClassifierPackage ####
### Not sure why this is here when we have SpeciesGroup in tblDeployment
tblDeploymentSoftware %>% 
  group_by(ClassifierPackage) %>% 
  count()

classifier <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/DataManager/Scripts/C10KClassifier.csv')
classifier <- classifier %>% 
  mutate(CONUS_10KM = as.character(CONUS_10KM))

tblPointLocation <- dbReadTable(con,'tblPointLocation')
tblSite <- dbReadTable(con,'tblSite')

dat <- dat %>% 
  select(-c(PointLocationID,DeploymentDate,LocationName,LocationName_GRTS)) %>% 
  left_join(tblDeployment,by=c('DeploymentID' = 'ID')) %>% 
  left_join(tblPointLocation,by=c('PointLocationID' = 'ID')) %>% 
  left_join(tblSite,by=c('SiteID' = 'ID')) %>% 
  left_join(classifier,by=c('SampleUnitID' = 'CONUS_10KM')) %>% 
  mutate(ClassifierPackage =
           case_when(!is.na(Classifier) ~ Classifier,
                     is.na(Classifier) & ParkCode == 'ID' & SoftwareID == 7 ~ 'Eastern Washington',
                     SoftwareID == 1 ~ 'Bats of North America, Idaho + TABR')) %>% 
  select(DeploymentID,SoftwareID,ContactID,Version,ClassifierPackage)

dat %>% 
  distinct(ClassifierPackage)

dat %>% 
  left_join(tblDeployment %>% 
              select(ID,PointLocationID),
            by = c('DeploymentID' = 'ID')) %>% 
  left_join(tblPointLocation %>% 
              select(ID,LocationName,LocationName_GRTS),
            by = c('PointLocationID' = 'ID')) %>% 
  filter(is.na(ClassifierPackage))



## SpeciesIncludedInAnalysis ####
# No values exist from previously, not sure what this should be
tblDeploymentSoftware %>% 
  group_by(SpeciesIncludedInAnalysis) %>% 
  count()


## MinNumPulses ####
tblDeploymentSoftware %>% 
  group_by(MinNumPulses) %>% 
  count()

dat <- dat %>% 
  mutate(MinNumPulses = 2)


## OtherSettings ####
tblDeploymentSoftware %>% 
  group_by(OtherSettings) %>% 
  count()

dat <- dat %>% 
  mutate(OtherSettings = 
           case_when(SoftwareID == 1 ~ '7 - 120 kHz, 2 - 500 ms, 500 ms max, advanced signal processing'))


## Created and Modified fields ####
dat <- dat %>% 
  mutate(CreatedBy = 'P. Emblidge',
         LastModifiedBy = 'P. Emblidge',
         CreatedDate = as.Date(Sys.time()),
         LastModifiedDate = as.Date(Sys.time()))


options(odbc.batch_rows = 1)
dbAppendTable(con,'tblDeploymentSoftware',dat)



### It's always a good idea to close database connections at the end of your session
#odbcCloseAll()
dbDisconnect(con)
