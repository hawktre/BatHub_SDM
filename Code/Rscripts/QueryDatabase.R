library(tidyverse)
library(DBI)
#library(tidyr)
#library(readxl)
#library(sf)
#library(stringr)
#library(odbc)

### RStudio uses 64 bit R by default
### MS Access and associated Drivers are 32 bit, causing mismatch errors
### Use 32 bit R for database connectivity
###   Tools > Global Options > General > R version:
### But remember to set back to 64 bit when done

### DBI package (R Database Interface) is an option for interacting with data in database,
### and has better tools for adding data to database, and is also faster that RODBC tools
con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=/Users/trentvanhawkins/Library/Mobile Documents/com~apple~CloudDocs/Documents/OnBelay/Projects/BatHub/BatHub_SDM/DataRaw.nosync/Database/PNW_BatHub_Database.accdb")

### Show the database tables
dbListTables(con)

### Read in database tables
tblSite <- dbReadTable(con, 'tblSite')
tblPointLocation <- dbReadTable(con, 'tblPointLocation')
head(tblPointLocation)
tblPointLocation <- tblPointLocation %>% 
  select(1:3)
tblDeployment <- dbReadTable(con, 'tblDeployment')
head(tblDeployment)
tblDeployment <- tblDeployment %>% 
  select(1,2)
tblDeploymentDetection7 <- dbReadTable(con, 'tblDeploymentDetection7')
head(tblDeploymentDetection7)
names(tblDeploymentDetection7)
tblDeploymentDetection7 <- tblDeploymentDetection7 %>% 
  select(1,2,3,5,38,39)

### Join database tables
dat <- tblDeploymentDetection7 %>% 
  left_join(tblDeployment,
            by = c('DeploymentID' = 'ID')) %>% 
  left_join(tblPointLocation,
            by = c('PointLocationID' = 'ID')) %>% 
  select(9,3:6)
dat

### Read in bat list
bats <- read.csv('C:/Users/emblidgp/Desktop/BatList.csv') %>% 
  pull(Code)

### Identify Sample Unit of interest
SU <- '121346'

### Previous deployments at Site of interest
dat %>% 
  filter(str_detect(LocationName,SU)) %>% 
  distinct(LocationName,Night)


### Species confirmed at SU of interest
dat2 <- dat %>% 
  filter(str_detect(LocationName,SU) &
           ManualIDSpp2 %in% bats) %>% 
  select(LocationName,
         Night,
         'ManualIDSpp' = ManualIDSpp2) %>% 
  mutate(ManualIDSpp = as.character(ManualIDSpp))

dat1 <- dat %>% 
  filter(str_detect(LocationName,SU) &
           ManualIDSpp1 %in% bats) %>% 
  rename('ManualIDSpp' = ManualIDSpp1) %>% 
  add_row(dat2) %>% 
  mutate(detected = 1,
         Year = format(Night,'%Y')) %>% 
  distinct(LocationName,Year,ManualIDSpp,detected) %>% 
  add_row(data.frame(ManualIDSpp = bats[!bats %in% .$ManualIDSpp]),
          LocationName = unique(.$LocationName)[1],
          Year = unique(.$Year)[1]) %>% 
  pivot_wider(names_from = ManualIDSpp,values_from = detected,names_sort = T)

### Compile data not yet in database from vettingoutput files
year <- '2022'
state <- 'Washington'

# Load vetting output of intended year
tmp <- read_delim(paste0("C:/Users/emblidgp/Box/HERS_BatAcousticFiles/NABat/",state,"/Processed/",year,"/VettingOutput/",SU,"_Database.txt"))

# Identify species manually confirmed at StationLocation
tmp2 <- tmp %>% 
  filter(str_detect(ParentDir,SU) &
           `User|ManualIDSpp2` %in% bats) %>% 
  select(ParentDir,
         'Species Manual ID' = `User|ManualIDSpp2`) %>% 
  mutate(`Species Manual ID` = as.character(`Species Manual ID`))

tmp <- tmp %>% 
  filter(str_detect(ParentDir,SU) &
           `Species Manual ID` %in% bats) %>% 
  add_row(tmp2) %>% 
  mutate(Year = year,
         detected = 1) %>% 
  rename('LocationName' = ParentDir,
         'ManualIDSpp' = 'Species Manual ID') %>% 
  distinct(LocationName,Year,ManualIDSpp,detected) %>% 
  add_row(data.frame(ManualIDSpp = bats[!bats %in% .$ManualIDSpp]),
          LocationName = unique(.$LocationName)[1],
          Year = unique(.$Year)[1]) %>% 
  pivot_wider(names_from = ManualIDSpp,values_from = detected,names_sort = T)


### Combine database results with those pulled directly from VettingOutput (PullVettingOutput.R)
out <- dat1 %>% 
  add_row(tmp)
out <- out %>% 
  add_row(tmp)

write_csv(out,paste0("C:/Users/emblidgp/Desktop/",SU,"_TotalSpeciesRichness.csv"))



### Pull all EPFU
dat %>% 
  distinct(ManualIDSpp1)
dat %>% 
  distinct(ManualIDSpp2)

dat %>% 
  filter(toupper(ManualIDSpp1) == 'EPFU' |
           toupper(ManualIDSpp2) == 'EPFU')



####################################
### Pull data for Emily Jeffreys ###
####################################
str(dat)
sites <- c('124138',
           '123682',
           '123680',
           '125999',
           '125993',
           '126915',
           '124599')

bats <- read.csv('C:/Users/emblidgp/Desktop/BatList.csv') %>% 
  pull(Code)
bats

### This code checks to make sure we're not missing any confirmed species
dat %>% 
  filter(str_extract(LocationName, '\\d+') %in% sites) %>% 
  select(LocationName,Night,ManualIDSpp1,ManualIDSpp2) %>% 
  pivot_longer(cols = contains("Manual", ignore.case = T),
               names_to = NULL,
               values_to = "ManualID") %>% 
  mutate(ManualID = toupper(ManualID)) %>% 
  filter(!is.na(ManualID) &
           !(ManualID %in% bats)) %>% 
  distinct(ManualID)

### This code checks to make sure we're not missing any confirmed species
dat <- dat %>% 
  filter(str_extract(LocationName, '\\d+') %in% sites) %>% 
  select(LocationName,Night,ManualIDSpp1,ManualIDSpp2) %>% 
  pivot_longer(cols = contains("Manual", ignore.case = T),
               names_to = NULL,
               values_to = "ManualID") %>% 
  mutate(ManualID = toupper(ManualID)) %>% 
  filter(!is.na(ManualID) &
           ManualID %in% bats)

dat <- dat %>% 
  mutate(Detected = 1)

dat.richness <- dat %>% 
  pivot_wider(id_cols = c(LocationName,Night), names_from = ManualID, values_from = Detected, values_fn = min, values_fill = 0, names_sort = T) %>%
  arrange(LocationName)

dat <- dat.richness %>% 
  mutate(Night = format(Night, '%Y'),
         across(everything(), as.character)) %>% 
  rename('Year' = 'Night')


### 2020 Data
dat2020 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/2020_Analysis/Acoustic Output_OR_WA.xlsx', sheet = 'Sheet5')
dat2020 <- dat2020 %>% 
  filter(SampleUnit %in% sites) %>% 
  mutate(MonitoringNight = format(MonitoringNight, '%Y'),
         across(everything(), as.character)) %>% 
  rename('Year' = 'MonitoringNight',
         'LocationName' = 'LocationID') %>% 
  select(-c('State','SampleUnit'))

### 2021 Data
dat2021 <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/2021_Analysis/SppRichness2021.csv')
dat2021 <- dat2021 %>% 
  filter(CONUS10K %in% sites) %>% 
  mutate(Year = 2021,
         across(everything(), as.character)) %>% 
  rename(LocationName = Site) %>% 
  select(4,20,5:19)



names(dat)
names(dat2020)
names(dat2021)

dat.richness <- dat2020 %>% 
  add_row(dat2021) %>% 
  add_row(dat) %>% 
  mutate(Cell = str_extract(LocationName, '\\d+')) %>% 
  select(2,18,1,3:17) %>% 
  arrange(Cell, Year,str_extract(LocationName, '\\d_[NS][EW]'))

write_csv(dat.richness, 'C:/Users/emblidgp/Desktop/Results_WDFW_Wenatchee_2018_2021.csv', na = '0')



### It's always a good idea to close database connections at the end of your session
dbDisconnect(con)
