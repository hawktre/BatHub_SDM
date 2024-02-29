install.packages(c('tidyverse','DBI','readxl'))

library(tidyverse)
library(DBI)
library(readxl)

### RStudio uses 64 bit R by default
### MS Access and associated Drivers are 32 bit, causing mismatch errors
### Use 32 bit R for database connectivity
###   Tools > Global Options > General > R version:
### But remember to set back to 64 bit when done

con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=/Users/trentvanhawkins/Library/Mobile Documents/com~apple~CloudDocs/Documents/OnBelay/Projects/BatHub/BatHub_SDM/DataRaw.nosync/Database/PNW_BatHub_Database.accdb")

### Read in master SU list
CellTracker <- read_excel("C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2023.xlsx")

### Show the database tables
dbListTables(con)

### Read in database table
tblSite <- dbReadTable(con, 'tblSite')

tblSite <- tblSite %>% 
  mutate(SampleUnitID = as.numeric(SampleUnitID),
         SampleUnitID_GRTS = as.numeric(SampleUnitID_GRTS))

### 2022 data
dat22FieldMaps <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx')
dat22Paper <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx')

dat22 <- dat22FieldMaps %>% 
  select(State, `Sample Unit`) %>% 
  add_row(dat22Paper %>% 
            select(State, `Sample Unit`)) %>% 
  distinct()

tmp1 <- CellTracker %>% 
  select(CONUS_10KM,GRTS_ID)

site22 <- dat22 %>% 
  left_join(tmp1, by = c('Sample Unit' = 'GRTS_ID')) %>% 
  left_join(tmp1, by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(SampleUnitID = 
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         SampleUnitID_GRTS =
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID)) %>% 
  select(-c(2:4)) %>% 
  rename('ParkCode' = 'State') %>% 
  distinct()


### Sites to add
sites <- site22 %>% 
  filter(!(SampleUnitID %in% tblSite$SampleUnitID) &
           SampleUnitID > 100)

sites %>% 
  print(n=Inf)


####################
### BroadHabitat ###
####################
### Read in lookup table from database
tluBroadHabitat <- dbReadTable(con,'tluBroadHabitat')
### Read in SUs
SUs <- st_read("C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/conus_mastersample_10km_attributed/conus_mastersample_10km_attributed.shp")
### Create SU centroid points
SUs_centroid <- st_centroid(SUs)
### Read in National Vegetation Classification data
nvc <- read_stars("C:/Users/emblidgp/Downloads/gap_landfire_nationalterrestrialecosystems2011/gap_landfire_nationalterrestrialecosystems2011.tif")
### Read in NVC value definitions
code <- read_excel('C:/Users/emblidgp/Downloads/gap_landfire_nationalterrestrialecosystems2011/gaplf2011_conus_attributes_16dec2016.xlsx')
### Edit name of raster value
nvc <- setNames(nvc, "nvc")

### Add NVC value to new sites
# Create centroids of new SUs
tmp1 <- SUs_centroid[SUs_centroid$CONUS_10KM %in% sites$SampleUnitID,]
# Transform to match NVC data
tmp1 <- st_transform(tmp1,5070)
# Extract NVC values to CONUS centroids
tmp2 <- tmp1 %>% 
  mutate(nvc = st_extract(nvc,tmp1) %>% 
           pull(nvc)) %>% 
  st_drop_geometry() %>% 
  select(CONUS_10KM,nvc)
# Add nvc values, label, and BroadHabitatID to new SUs
sites <- sites %>% 
  left_join(tmp2,
            by = c('SampleUnitID' = 'CONUS_10KM')) %>% 
  left_join(code %>% 
              select(Value,NVC_FORM),
            by = c('nvc' = 'Value')) %>% 
  left_join(tluBroadHabitat,
            by = c('NVC_FORM' = 'Label')) %>% 
  rename('BroadHabitatID' = 'ID')
# Raise warning if any NVC values are not in tluBroadHabitat
if(any(is.na(sites$BroadHabitatID))){
  tmp3 <- sites %>% 
    filter(is.na(BroadHabitatID)) %>% 
    pull(NVC_FORM)
  warning(paste0("The following NVC values are not in tluBroadHabitat: ",paste(tmp3,collapse = ', ')))}else{
    sites <- sites %>% 
      select(-c('nvc','NVC_FORM'))}


### Look for discrepancies existing in Access Database
# Create centroids of SUs already in database
tmp1 <- SUs_centroid[SUs_centroid$CONUS_10KM %in% tblSite$SampleUnitID,]
# Transform to match NVC data
tmp1 <- st_transform(tmp1,5070)
# Extract NVC values to CONUS centroids
tmp2 <- tmp1 %>% 
  mutate(CONUS_10KM = as.character(CONUS_10KM),
         nvc = st_extract(nvc,tmp1) %>% 
           pull(nvc)) %>% 
  st_drop_geometry() %>% 
  select(CONUS_10KM,nvc)
# Add nvc values to tblSite to compare with existing values
tmp3 <- tblSite %>% 
  left_join(tmp2,
            by = c('SampleUnitID' = 'CONUS_10KM')) %>% 
  left_join(tluBroadHabitat,
            by = c('BroadHabitatID' = 'ID')) %>% 
  left_join(code %>% 
              select(Value,NVC_CLASS,NVC_FORM),
            by = c('nvc' = 'Value')) %>% 
  select(SampleUnitID,Label,NVC_CLASS,NVC_FORM)
# Investigate SUs in the database with BoradHabitat that does not match NVC
tmp3 %>% 
  filter(Label != NVC_FORM)



#####################
### FederalAgency ###
#####################
### The organization responsible for sampling
tblSite %>% 
  distinct(FederalAgency)

### Check that existing records match the master list
tblSite %>% 
  left_join(CellTracker, by = c('SampleUnitID' = 'CONUS_10KM')) %>% 
  filter(ParkCode != NABatProject) %>% 
  select(SampleUnitID,SampleUnitID_GRTS,ParkCode,NABatProject)

sites <- sites %>% 
  left_join(CellTracker %>% 
              select(CONUS_10KM,GRTS_ID,Agency),
            by = c('SampleUnitID' = 'CONUS_10KM')) %>% 
  select(-GRTS_ID) %>% 
  rename('FederalAgency' = 'Agency')

sites %>% 
  distinct(FederalAgency)
sites %>% 
  filter(is.na(FederalAgency))
### Sites inadvertently surveyed outside the intended cell should be assigned 'FederalAgency' of intended cell
sites <- sites %>% 
  mutate(FederalAgency =
           case_when(SampleUnitID == '101940' ~ sites$FederalAgency[sites$SampleUnitID_GRTS == '2081'],
                     SampleUnitID == '104266' ~ sites$FederalAgency[sites$SampleUnitID_GRTS == '289'],
                     SampleUnitID == '95936' ~ sites$FederalAgency[sites$SampleUnitID_GRTS == '3914'],
                     SampleUnitID == '101481' ~ sites$FederalAgency[sites$SampleUnitID_GRTS == '1418'],
                     TRUE ~ FederalAgency))
sites


#########################
### Format for upload ###
#########################
head(sites)
sites <- sites %>% 
  mutate(SiteCode = as.character(SampleUnitID),
         SampleUnitID = as.character(SampleUnitID),
         SampleUnitID_GRTS = as.character(SampleUnitID_GRTS),
         SampleDesignID = 1,
         SiteDescription = NA_character_,
         CreatedBy = 'P. Emblidge',
         LastModifiedBy = 'P. Emblidge') %>% 
  select(SiteCode,ParkCode,SampleDesignID,SampleUnitID,SampleUnitID_GRTS,FederalAgency,SiteDescription,BroadHabitatID,CreatedBy,LastModifiedBy)


#######################################
### Relationships and lookup tables ###
#######################################
### tblSite::ID               -   tblSiteStateCounty::SiteID
tblSiteStateCounty <- dbReadTable(con,'tblSiteStateCounty')
head(tblSiteStateCounty)
### SiteID is an important field in this table and is automatically populated when data added to tblSite
### Therefore, update tblSiteStateCounty after tblSite


### tblSite::BroadHabitatID   -   tluBroadHabitat::ID
sites %>% 
  filter(!(BroadHabitatID %in% tluBroadHabitat$ID))


### tblSite:ParkCode          -   tluPark::ParkCode
unique(sites$ParkCode)
dbReadTable(con,'tluPark')
### Add new record to table
dbExecute(con, "INSERT INTO tluPark (ParkCode, ParkName)
         VALUES ('NV', 'Nevada NABat')")


### tblSite::SampleDesignID   -   tluSampleDesign::ID
unique(sites$SampleDesignID)
dbReadTable(con,'tluSampleDesign')


### Add new sites to tblSite
head(sites)
dim(sites)
options(odbc.batch_rows = 1) # This option needs to be set to append multiple rows (Access-specific)
dbAppendTable(con,'tblSite',sites)


### It's always a good idea to close database connections at the end of your session
dbDisconnect(con)
