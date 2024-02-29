### Uploading vetted output results to Access PNW_BatHub_Database_AcousticOutput.accdb tblDeploymentDetection7
### Download the current version of the database to your Desktop

# Clear your workspace and install and load packages ----
rm(list = ls())
library(tidyverse)
library(sf)
library(DBI)
library(readxl)
library(lubridate)

# Identify yourself and year of data to upload ----
onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")



# Connect to both databases ----
con1 <- dbConnect(odbc::odbc(),
                  .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/",onid,"/Desktop/PNW_BatHub_Database.accdb"))
con2 <- dbConnect(odbc::odbc(),
                  .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/",onid,"/Desktop/PNW_BatHub_Database_AcousticOutput.accdb"))

# Check existing data ----
# Check What data has already been uploaded
tblDeploymentDetection7 <- dbReadTable(con2,'tblDeploymentDetection7', check.names = F)
tblDeploymentDetection7 %>% 
  group_by(Night) %>% 
  count() %>% 
  arrange(Night) %>% 
  print(n=Inf)


# Summarize existing database Deployments
tblPointLocation <- dbReadTable(con1,'tblPointLocation')
tblDeployment <- dbReadTable(con1,'tblDeployment')
Deployments <- tblDeployment %>% 
  select(ID,PointLocationID,DeploymentDate,RecoveryDate) %>% 
  left_join(tblPointLocation %>% 
              select(ID,LocationName,LocationName_GRTS),
            by = c('PointLocationID' = 'ID'))

Deployments %>% 
  filter(str_detect(DeploymentDate,year))


# Read in Vetted Acoustic Data ----
# This code will need to be edited as Acoustic Output files are divided by Project in the future
# As of 2023 we were not adding USFS_Region4 data to our database
dat_PNW_Sonobat <- read_csv(paste0("C:/Users/",onid,"/Box/HERS_Working/Bats/Analysis_NABat/",year,"_Analysis/AcousticOutput_NABat_PNW_SonoBat_",year,".csv"),
                            col_types = cols(.default = 'c')) %>% 
  mutate(LocationName = paste0(CONUS,'_',str_extract(Site,'[NS][EW]\\d')))
head(dat_PNW_Sonobat)

dat_USFWS_Sonobat <- read_csv(paste0("C:/Users/",onid,"/Box/HERS_Working/Bats/Analysis_NABat/",year,"_Analysis/AcousticOutput_NABat_USFWS_SonoBat_",year,".csv"),
                              col_types = cols(.default = 'c')) %>% 
  mutate(LocationName = paste0(CONUS,'_',str_extract(Site,'[NS][EW]\\d')))
head(dat_USFWS_Sonobat)

dat_Kaleidoscope <- read_csv(paste0("C:/Users/",onid,"/Box/HERS_Working/Bats/Analysis_NABat/",year,"_Analysis/AcousticOutput_NABat_PNW_Kaleidoscope_",year,".csv"),
                                 col_types = cols(.default = 'c')) %>% 
  mutate(LocationName = paste0(CONUS,'_',str_extract(Site,'[NS][EW]\\d')))
head(dat_Kaleidoscope)


# Prepare and Upload SonoBat results----
dat_Sonobat <- dat_PNW_Sonobat %>% 
  add_row(dat_USFWS_Sonobat)

## Format For Database Upload ----
### DeploymentID ----
# Identify each unique LocationID and MonitoringNight
tmp1 <- dat_Sonobat %>% 
  distinct(LocationName,MonitoringNight) %>% 
  mutate(DeploymentID = NA_integer_)


# Loop through each unique LocationID and MonitoringNight and pull DeploymentID from Access Database
for(i in 1:length(tmp1$LocationName)){
  tmp2 <- Deployments %>% 
    filter(LocationName == tmp1$LocationName[i] &
             as.Date(DeploymentDate) <= tmp1$MonitoringNight[i] &
             as.Date(RecoveryDate) > tmp1$MonitoringNight[i])
  if(length(tmp2$ID)==1){
    tmp1$DeploymentID[i] <- tmp2$ID
  }else{tmp1$DeploymentID[i] <- -99999}}

# Look for any acoustic data without a deployment ID, investigate all errors and make corrections where appropriate, then re-run to attach DeploymentID
tmp1 %>% 
  filter(is.na(DeploymentID))
tmp1 %>% 
  filter(DeploymentID==-99999) %>% 
  arrange(LocationName,MonitoringNight)


# Make some 2022 data corrections, then rerun lines 109 - 129
dat_Sonobat <- dat_Sonobat %>% 
  filter(!(LocationName == '102386_NW1' &
             MonitoringNight == '2022-07-01') &
           !(LocationName == '107020_SE2' &
               MonitoringNight == '2022-07-10') &
           !(LocationName == '107481_NE1' &
               MonitoringNight == '2022-06-30') &
           !(LocationName == '107481_SE1' &
               MonitoringNight == '2022-06-30') &
           !(LocationName == '107481_SW1' &
               MonitoringNight == '2022-06-30') &
           !(LocationName == '109335_NW2' &
               MonitoringNight == '2022-07-31') &
           LocationName != '96350_NA') %>% 
  mutate(MonitoringNight =
           case_when(LocationName == '117177_SW1' ~ '2022-08-27',
                     LocationName == '117182_NE1' ~ '2022-08-26',
                     LocationName == '117633_NE2' ~ '2022-08-24',
                     LocationName == '118099_NE2' ~ '2022-08-25',
                     TRUE ~ MonitoringNight))

# Check to make sure data looks good, no missing values or values outside of the expected range
head(tmp1)
tmp1 %>% 
  distinct(LocationName) %>% 
  arrange(LocationName) %>% 
  print(n=Inf)
tmp1 %>% 
  distinct(MonitoringNight) %>% 
  arrange(MonitoringNight) %>% 
  print(n=Inf)
tmp1 %>% 
  distinct(DeploymentID) %>% 
  arrange(DeploymentID) %>% 
  print(n=Inf)

# Join DeploymentID to Acoustic Output data
dat_Sonobat <- dat_Sonobat %>% 
  left_join(tmp1)

# Double check DeploymentID
dat_Sonobat %>% 
  filter(is.na(DeploymentID))
dat_Sonobat %>% 
  filter(DeploymentID==-99999)


### Night ----
dat_Sonobat %>% 
  distinct(MonitoringNight) %>% 
  arrange(MonitoringNight) %>% 
  print(n=Inf)

dat_Sonobat %>% 
  filter(is.na(MonitoringNight)) %>% 
  distinct(`User|Comments`)

# Join tblDeployment and calculate Night from MonitoringNight, or DeploymentNight if MonitoringNight = NA 
dat_Sonobat <- dat_Sonobat %>% 
  left_join(tblDeployment %>% 
              select(ID,DeploymentDate),
            by = c('DeploymentID' = 'ID')) %>% 
  mutate(Night = 
           case_when(is.na(MonitoringNight) ~ as.character(as.Date(DeploymentDate)),
                     TRUE ~ MonitoringNight)) %>% 
  select(-DeploymentDate)


dat_Sonobat %>% 
  distinct(Night) %>% 
  arrange(Night) %>% 
  print(n=Inf)

### Calculate remaining fields ----
# Select only the fields included in the database table
names(tblDeploymentDetection7)
names(dat_Sonobat)

dat_Sonobat <- dat_Sonobat %>% 
  select(DeploymentID,Night,Path,Filename,HiF,LoF,SppAccp,Prob,13:43)


# Add Created/Modified Fields
dat_Sonobat <- dat_Sonobat %>% 
  mutate(CreatedBy = 'P. Emblidge',
         CreatedDate = as.Date(Sys.time(),tz = 'America/Los_Angeles'),
         LastModifiedBy = 'P. Emblidge',
         LastModifiedDate = as.Date(Sys.time(),tz = 'America/Los_Angeles'))


# Format column names
names(tblDeploymentDetection7)
names(dat_Sonobat)

names(dat_Sonobat) <- names(tblDeploymentDetection7)[-1]

# If SonoBatch was set to 'output null classification results as "x"' for any deployments the x's causes problems with upload. 
# Convert everything to character, then replace 'x' with NA
dat_Sonobat <- dat_Sonobat %>% 
  mutate(across(1:length(names(.)),
                ~ as.character(.))) %>% 
  mutate(across(1:length(names(.)),
                ~ case_when(. == 'x' ~ NA,
                            . != 'x' ~ .)))

## Upload to Database ----
# Read in tblDeploymentDetection7 prior to upload
tblDeploymentDetection7 <- dbReadTable(con2,'tblDeploymentDetection7')

# Upload to database
# Upload records to tblDeploymentDetection7 in batches of 40,000 records at a time
# This allows us to keep better track of progress if the upload fails, and seems to reduce likelihood of hitting memory limit errors. 
options(odbc.batch_rows = 1)
for(i in 1:ceiling(length(dat_Sonobat$DeploymentID)/40000)){
  dbAppendTable(con2,'tblDeploymentDetection7',dat_Sonobat[(((i-1)*40000)+1):min(i*40000,length(dat_Sonobat$DeploymentID)),])
}

# Read in updated Database
tblDeploymentDetection <- dbReadTable(con2,'tblDeploymentDetection7')

# Check to make sure all records properly uploaded
dim(tblDeploymentDetection7)[[1]] + dim(dat_Sonobat)[[1]] == dim(tblDeploymentDetection)[[1]]

### If data upload returns an error ----
# If dbAppendTable returns an ERROR, you need to check if some of your data was imported prior to the error
# If so, either remove the partial upload, or remove the uploaded records from the next run, so you don't end up with duplicate records.
tblDeploymentDetection7 <- dbReadTable(con2,'tblDeploymentDetection7')
dim(tblDeploymentDetection7)

# Remove partial upload records that were interrupted by Error
# Identify list of DeploymentDetection7ID to delete
tmp1 <- tblDeploymentDetection7 %>% 
  filter(str_detect(as.character(Night),year)) %>% 
  pull(ID)
length(tmp1)

# Maximum number of records you can delete at one time is 40,000, so loop your deletion in chunks of 40,000 records at a time
for(i in 1:ceiling(length(tmp1)/40000)){
  dbExecute(con2,
            paste0("DELETE * FROM tblDeploymentDetection7 WHERE ID in (",paste(tmp1[(((i-1)*40000)+1):min(i*40000,length(tmp1))],collapse = ','),")"))
}


# Prepare and Upload Kaleidoscope results ----
dat_Kaleidoscope %>% 
  distinct(LocationName) %>% 
  print(n=Inf)

head(dat_Kaleidoscope)


## Format For Database Upload ----
### DeploymentID ----
# Convert DATE.12 to date object
dat_Kaleidoscope <- dat_Kaleidoscope %>% 
  mutate(DATE.12 = 
           case_when(is.na(mdy(DATE.12)) ~ as.Date(DATE.12),
                     TRUE ~ mdy(DATE.12)))

# Check to make sure all Monitoring Nights are in the same unambiguous format
dat_Kaleidoscope %>% 
  distinct(DATE.12) %>% 
  print(n=Inf)

# Create temporary data.frame for each distinct Station Location and Monitoring Night
tmp1 <- dat_Kaleidoscope %>% 
  distinct(LocationName,`DATE.12`) %>% 
  mutate(DeploymentID = NA_integer_)

# Loop through each unique LocationID and MonitoringNight and pull DeploymentID from Access Database
for(i in 1:length(tmp1$LocationName)){
  tmp2 <- Deployments %>% 
    filter(LocationName == tmp1$LocationName[i] &
             as.Date(DeploymentDate) <= tmp1$DATE.12[i] &
             as.Date(RecoveryDate) > tmp1$DATE.12[i])
  if(length(tmp2$ID)==1){
    tmp1$DeploymentID[i] <- tmp2$ID
  }else{tmp1$DeploymentID[i] <- -99999}}

# Look for any erroneous DeploymentIDs
tmp1 %>% 
  filter(is.na(DeploymentID))
tmp1 %>% 
  filter(DeploymentID==-99999)

# Check to make sure data looks good, no missing values or values outside of the expected range
head(tmp1)
tmp1 %>% 
  distinct(LocationName) %>% 
  arrange(LocationName) %>% 
  print(n=Inf)
tmp1 %>% 
  distinct(DATE.12) %>% 
  arrange(DATE.12) %>% 
  print(n=Inf)
tmp1 %>% 
  distinct(DeploymentID) %>% 
  arrange(DeploymentID) %>% 
  print(n=Inf)

# Join DeploymentID to dat_Kaleidoscope
dat_Kaleidoscope <- dat_Kaleidoscope %>% 
  left_join(tmp1)

# Double check DeploymentID values
dat_Kaleidoscope %>% 
  filter(is.na(DeploymentID))
dat_Kaleidoscope %>% 
  filter(DeploymentID==-99999)


### Night ----
dat_Kaleidoscope %>% 
  distinct(`DATE.12`) %>% 
  print(n=Inf)

dat_Kaleidoscope %>% 
  filter(is.na(DATE.12))

dat_Kaleidoscope <- dat_Kaleidoscope %>% 
  mutate(Night = as.Date(`DATE.12`))

### Calculate remaining fields ----
dat_Kaleidoscope <- dat_Kaleidoscope %>% 
  mutate(Path = paste(OUTDIR,FOLDER,`IN.FILE`,sep = '\\'),
         Filename = paste0(paste(GRTS,'_',str_extract(LocationName,'[NS][EW]\\d')),'_',`OUT.FILE.ZC`),
         ManualIDSpp1 = `MANUAL.ID`,
         Notes = 'Zero-crossing data forced into this table, should probably be removed and added to a separate DeploymentDetection table in the future.',
         CreatedBy = 'P. Emblidge',
         CreatedDate = as.Date(Sys.time(),tz = 'America/Los_Angeles'),
         LastModifiedBy = 'P. Emblidge',
         LastModifiedDate = as.Date(Sys.time(),tz = 'America/Los_Angeles')) %>% 
  select(DeploymentID,Night,Path,Filename,ManualIDSpp1,Notes,CreatedBy,CreatedDate,LastModifiedBy,LastModifiedDate)

## Upload to Databse ----
# Read in tblDeploymentDetection7 prior to upload
tblDeploymentDetection7 <- dbReadTable(con2,'tblDeploymentDetection7')

# Upload to Database
options(odbc.batch_rows = 1)
dbAppendTable(con2,'tblDeploymentDetection7',dat_Kaleidoscope)

# Read in updated Database
tblDeploymentDetection <- dbReadTable(con2,'tblDeploymentDetection7')

# Check to make sure all records properly uploaded
dim(tblDeploymentDetection7)[[1]] + dim(dat_Kaleidoscope)[[1]] == dim(tblDeploymentDetection)[[1]]


### If data upload returns an error ----
# If dbAppendTable returns an ERROR, you need to check if some of your data was imported prior to the error
# If so, either remove the partial upload, or remove the uploaded records from the next run, so you don't end up with duplicate records.
tblDeploymentDetection7 <- dbReadTable(con2,'tblDeploymentDetection7')

# Remove partial upload records that were interrupted by Error
tmp1 <- tblDeploymentDetection %>% 
  filter(str_detect(Night,year)) %>% 
  pull(ID)
length(tmp1)

dbExecute(con2,
          paste0("DELETE * FROM tblDeploymentDetection7 WHERE ID in (",paste(tmp1,collapse = ','),")"))


# Make sure there are no duplicate records in tblDeploymentDetection7 ----
tblDeploymentDetection7 %>% 
  group_by(DeploymentID,Night,Path,Filename) %>% 
  count() %>% 
  filter(n > 1 & 
           str_detect(Night,year))


# Disconnect from the databases ----
dbDisconnect(con1)
dbDisconnect(con2)
