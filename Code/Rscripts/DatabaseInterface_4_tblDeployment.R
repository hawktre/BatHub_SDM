library(tidyverse)
library(sf)
library(DBI)
library(readxl)
library(lutz)
library(sfheaders)

########################################################################
### RStudio uses 64 bit R by default
### MS Access and associated Drivers are 32 bit, causing mismatch errors
### Use 32 bit R for database connectivity
###   Tools > Global Options > General > R version:
### But remember to set back to 64 bit when done
########################################################################

### Establish database connection
con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/emblidgp/Desktop/PNW_BatHub_Database.accdb")

### Read in database tables
tblDeployment <- dbReadTable(con,'tblDeployment')
tblDeploymentSoftware <- dbReadTable(con,'tblDeploymentSoftware')
tblDeploymentContact <- dbReadTable(con,'tblDeploymentContact')
tblRecoveryContact <- dbReadTable(con,'tblRecoveryContact')
tblPointLocation <- dbReadTable(con,'tblPointLocation')
tblSite <- dbReadTable(con,'tblSite')

### Read in database lookup tables
tluAudioLogger <- dbReadTable(con,'tluAudioLogger')
tluContact <- dbReadTable(con,'tluContact')
tluSpeciesGroup <- dbReadTable(con,'tluSpeciesGroup')
tluDistanceRange <- dbReadTable(con,'tluDistanceRange')
tluMicrophoneType <- dbReadTable(con,'tluMicrophoneType')
tluOrientation <- dbReadTable(con,'tluOrientation')
tluClutterType <- dbReadTable(con,'tluClutterType')

### Read in CONUS/GRTS conversion table
CellList <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/GIS/NABat Grid Sampling Frame/complete_conus_mastersample_10km/complete_conus_mastersample_10km.xlsx') %>% 
  select(1:2)

### Read in metadata
dat1 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_FieldMaps.xlsx')
dat2 <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/NABatMetadata2022_PaperDatasheets.xlsx')

# Make sure fields match
names(dat1[!names(dat1) %in% names(dat2)])
names(dat2[!names(dat2) %in% names(dat1)])


dat <- dat1 %>% 
  filter(!`Sample Unit` %in% c(20,30,33,34,37,41,44,45)) %>% 
  mutate(`Detector Serial No.` = as.character(`Detector Serial No.`),
         Latitude =
           case_when(!is.na(y) ~ y,
                     TRUE ~ Latitude),
         Longitude = 
           case_when(!is.na(x) ~ x,
                     TRUE ~ Longitude),
         'Microphone Model' = NA,
         SAMP.FREQ = NA,
         PreTrig = NA,
         RecLen = NA,
         HP_Filter = NA,
         AUTOREC = NA,
         T.SENSE = NA,
         'Input Gain' = NA,
         'Trig Level' = NA,
         Interval = NA,
         'Trigger window length' = NA,
         'Maximum file length' = NA,
         FieldMaps = T) %>% 
  select(-c(CreationDate,Creator,EditDate,Editor,x,y)) %>% 
  add_row(dat2 %>% 
            filter(`Sample Unit` != 96350) %>% 
            mutate(FieldMaps = F) %>% 
            select(-c(Comments_BatHub,Datum,DepTemp,DepSky,DepWind,RecTemp,RecSky,RecWind),
                   'Altitude' = `Altitude (m)`)) %>% 
  left_join(CellList, by = c('Sample Unit' = 'GRTS_ID')) %>% 
  left_join(CellList, by = c('Sample Unit' = 'CONUS_10KM')) %>% 
  mutate(SampleUnitID = 
           case_when(State == 'ID' ~ CONUS_10KM,
                     TRUE ~ `Sample Unit`),
         SampleUnitID_GRTS =
           case_when(State == 'ID' ~ `Sample Unit`,
                     TRUE ~ GRTS_ID),
         LocationName = paste0(SampleUnitID,'_',Quad,`Quad Number`),
         LocationName_GRTS = paste0(SampleUnitID_GRTS,'_',Quad,`Quad Number`)) %>% 
  select(-c(`Sample Unit`,Quad,`Quad Number`,CONUS_10KM,GRTS_ID))

# Identify Station Locations surveyed multiple times
dat %>% 
  filter(LocationName %in% (dat %>% 
                              group_by(LocationName) %>% 
                              count() %>% 
                              filter(n>1) %>% 
                              pull(LocationName))) %>% 
  select(LocationName,`Deployment Date`) %>% 
  arrange(LocationName,`Deployment Date`)

# Add SiteDeployment
dat <- dat %>% 
  mutate(SiteDeployment =
           case_when(LocationName == '100552_SE1' & as.Date(`Deployment Date`) == as.Date('2022-08-29') ~ 2,
                     LocationName == '100552_SW1' & as.Date(`Deployment Date`) == as.Date('2022-08-29') ~ 2,
                     LocationName == '104648_NE1' & as.Date(`Deployment Date`) == as.Date('2022-07-27') ~ 2,
                     LocationName == '104648_NW2' & as.Date(`Deployment Date`) == as.Date('2022-07-27') ~ 2,
                     LocationName == '105578_NW1' & as.Date(`Deployment Date`) == as.Date('2022-07-26') ~ 2,
                     LocationName == '105579_SE1' & as.Date(`Deployment Date`) == as.Date('2022-07-25') ~ 2,
                     LocationName == '110673_NW2' & as.Date(`Deployment Date`) == as.Date('2022-08-15') ~ 2,
                     LocationName == '111606_SW3' & as.Date(`Deployment Date`) == as.Date('2022-06-15') ~ 2,
                     LocationName == '113478_NE1' & as.Date(`Deployment Date`) == as.Date('2022-07-21') ~ 2,
                     LocationName == '97272_NE1' & as.Date(`Deployment Date`) == as.Date('2022-08-22') ~ 2,
                     TRUE ~ 1))

dat %>% 
  filter(SiteDeployment==2)


## PointLocationID ####
dat <- dat %>% 
  left_join(tblPointLocation %>%
              select(LocationName,ID),
            by = 'LocationName') %>% 
  rename('PointLocationID' = 'ID')

# Make sure all records have a PointLocationID value
dat %>% 
  distinct(PointLocationID) %>% 
  arrange(PointLocationID) %>% 
  print(n=Inf)

dat %>% 
  filter(is.na(PointLocationID))
dat %>% 
  filter(PointLocationID=='')


## AudioLoggerID ####
# Read in DetectorCheck to see if we need to make any corrections in metadata
DC_OR <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/DetectorCheck_OR_2022.csv')
DC_WA <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/DetectorCheck_WA_2022.csv')
DC_ID <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/Analysis_NABat/2022_Analysis/DetectorCheck_ID_2022.csv')

DC <- DC_OR %>% 
  select(CONUS,GRTS,Site,SiteDeployment,DetectorType,DetectorSN,fileDetectorSN) %>% 
  add_row(DC_WA %>% 
            select(CONUS,GRTS,Site,SiteDeployment,DetectorType,DetectorSN,fileDetectorSN)) %>% 
  add_row(DC_ID %>% 
            select(CONUS,GRTS,Site,SiteDeployment,DetectorType,DetectorSN,fileDetectorSN)) %>% 
  mutate(LocationName = paste0(CONUS,str_extract(Site,"_[NS][EW]\\d"))) %>% 
  select(LocationName,SiteDeployment,DetectorType,DetectorSN,fileDetectorSN)


# Do we need to make any corrections in metadata
DC %>% 
  filter(DetectorSN != fileDetectorSN &
           fileDetectorSN != 'ERROR' &
           !str_detect(fileDetectorSN,DetectorSN))

# Join DetectorCheck to dat
dat <- dat %>% 
  left_join(DC %>% 
              select(-c(DetectorType,DetectorSN)))

# Identify any Detector Type values that do not match the format of tluAudioLogger,
# Make sure these are corrected in the step below
dat %>% 
  filter(!`Detector Type` %in% unique(tluAudioLogger$Model)) %>% 
  distinct(`Detector Type`)


# Format Detector Type and Serial Number
dat <- dat %>% 
  mutate(`Detector Type` =
           case_when(`Detector Type` == 'D500x' ~ 'D500X',
                     `Detector Type` == 'SM2' ~ 'SM2BAT',
                     `Detector Type` == 'Titley Scientific Swift' |
                       `Detector Type` == 'TITLEY AnaBat Swift' ~ 'Anabat Swift',
                     `Detector Type` == 'AnaBat Express' ~ 'Anabat Express',
                     `Detector Type` == 'Song Meter Mini Bat' ~ 'SMMiniBat',
                     TRUE ~ `Detector Type`),
         `Detector Serial No.` =
           case_when(fileDetectorSN != 'ERROR' & 
                       !is.na(fileDetectorSN) ~ fileDetectorSN,
                     `Detector Type` == 'D500X' ~ str_pad(`Detector Serial No.`,5,pad = 0),
                     `Detector Type` == 'SM4BAT' &
                       nchar(`Detector Serial No.`) == 2 ~ paste0('S4U000',`Detector Serial No.`),
                     `Detector Type` == 'SM4BAT' &
                       nchar(`Detector Serial No.`) == 4 ~ paste0('S4U0',`Detector Serial No.`),
                     `Detector Type` == 'SM4BAT' &
                       nchar(`Detector Serial No.`) == 5 ~ paste0('S4U',`Detector Serial No.`),
                     TRUE ~ `Detector Serial No.`))

# Identify `Detector Serial No.` that is not in standard format
dat %>% 
  distinct(`Detector Type`)

dat %>% 
  filter(`Detector Type` == 'D500X' & !str_detect(`Detector Serial No.`,'^\\d{5}$') |
           `Detector Type` == 'SM4BAT' & !str_detect(`Detector Serial No.`,'^S4U\\d{5}$') |
           `Detector Type` == 'SM3BAT' & !str_detect(`Detector Serial No.`,'^SM3\\d{5}$') |
           `Detector Type` == 'SMMiniBat' & !str_detect(`Detector Serial No.`,'^SMU\\d{5}$') |
           `Detector Type` == 'Anabat Swift' & !str_detect(`Detector Serial No.`,'^\\d{6}$') |
           `Detector Type` == 'Anabat Express' & !str_detect(`Detector Serial No.`,'^\\d{6}$')) %>% 
  select(LocationName,SiteDeployment,`Detector Type`,`Detector Serial No.`,fileDetectorSN)

# Make corrections
dat <- dat %>% 
  mutate(`Detector Serial No.` = 
           case_when(LocationName == '94500_SW1' &
                       SiteDeployment == 1 ~ '52086',
                     `Detector Type` == 'D500X' & 
                       !str_detect(`Detector Serial No.`,'^\\d{5}$') ~ str_extract(`Detector Serial No.`,'\\d{5}'),
                     TRUE ~ `Detector Serial No.`))

# Join AudioLoggerID from tluAudioLogger
dat <- dat %>% 
  left_join(tluAudioLogger %>% 
              select('AudioLoggerID' = ID,
                     SerialNum,
                     Model),
            by = c('Detector Type' = 'Model',
                   'Detector Serial No.' = 'SerialNum'))


# Identify new AudioLoggers that are not currently in tluAudioLogger
tluAudioLogger.new <- dat %>% 
  filter(is.na(AudioLoggerID) |
           AudioLoggerID == '') %>% 
  select('SerialNum' = `Detector Serial No.`,
         'Model' = `Detector Type`,
         'Agency' = `Deployment Agency`) %>% 
  distinct()

tluAudioLogger.new %>% 
  print(n=Inf)

# Have any new detectors been deployed by multiple agencies
dat %>% 
  filter(`Detector Serial No.` %in% (tluAudioLogger.new %>%
                                       group_by(Model,SerialNum) %>% 
                                       count %>% 
                                       filter(n>1) %>% 
                                       pull(SerialNum))) %>% 
  select(LocationName,`Detector Type`,`Detector Serial No.`,`Deployment Date`,`Deployment Contact(s)`,`Deployment Agency`) %>% 
  arrange(`Detector Type`,`Detector Serial No.`,`Deployment Date`)

# Remove detectors deployed by the non-owning agency, identified through above code
tluAudioLogger.new <- tluAudioLogger.new %>% 
  filter(!((SerialNum == '52299' & Agency == 'ODFW') |            # D500X 52299 belongs to BLM
             (SerialNum == 'S4U10987' & Agency == 'OSU')))        # SM4BAT S4U10987 belongs to NPS

# Make sure Agency is in same format as tluAudioLogger
dat %>% 
  filter(`Detector Serial No.` %in% (tluAudioLogger.new %>% 
                                       filter(!Agency %in% (tluAudioLogger %>% 
                                                              distinct(str_extract(Code,"[^-]+")) %>% 
                                                              pull())) %>% 
                                       pull(SerialNum))) %>% 
  distinct(`Detector Type`,`Detector Serial No.`,`Deployment Agency`,`Deployment Contact(s)`) %>% 
  arrange(`Detector Type`,`Detector Serial No.`) %>% 
  print(n=Inf)


# Format Agency, Code, and Manufacturer
tluAudioLogger.new <- tluAudioLogger.new %>% 
  mutate(Agency =
           case_when(Agency == 'Idaho Army National Guard' ~ 'IDARNG',
                     Agency == 'Idaho National Laboratory' |
                       Agency == 'INL' ~ 'DOE',
                     SerialNum %in% c('S4U08549','S4U08552') ~ 'IDARNG',
                     TRUE ~ Agency),
         Code = paste0(Agency,'-',SerialNum),
         Manufacturer =
           case_when(Model == 'Anabat Swift' ~ 'Titley Scientific',
                     Model == 'D500X' ~ 'Pettersson',
                     str_detect(Model,'^SM') ~ 'Wildlife Acoustics'),
         CreatedBy = 'P. Emblidge',
         LastModifiedBy = 'P. Emblidge') %>% 
  select(Code,SerialNum,Model,Manufacturer,CreatedBy,LastModifiedBy)


# Are any of my new loggers in tluAudioLogger as something else?
tluAudioLogger.new %>% 
  filter(SerialNum %in% tluAudioLogger$SerialNum)

### Firmware is a mess in existing database
### Only a single record for each detector, with more recently added records missing Firmware
### Good database design would not have a volatile field in a table describing hardware
### Firmware should probably be it's own lookup table to reduce redundancy
### Therefore I am going to ignore Firmware for now, I can always add it later if needed.

### Checking Firmware from acoustic files on Box for tluAudioLogger::ID = 172, (tluAudioLogger::Firmware = 2.2.8)
# 106544_NW1  6/8/2016 V2.2.8
# 119485_SW3  8/7/2019 V2.3.2
### Confirmation that Firmware field of tluAudioLogger is inaccurate in existing database and should either be removed or reformatted. 


# NA values are not allowed in tluAudioLogger::SerialNum
# I want just one record for each detector type with unknown serial number, ignoring Owner
tluAudioLogger.new %>% 
  filter(if_any(everything(), ~ is.na(.)))

tluAudioLogger.new <- tluAudioLogger.new %>% 
  mutate(Code = 
           case_when(is.na(SerialNum) ~ Model,
                     TRUE ~ Code),
         SerialNum =
           case_when(is.na(SerialNum) ~ 'Unknown',
                     TRUE ~ SerialNum))


### Add new records to tluAudioLogger
options(odbc.batch_rows = 1)
#dbAppendTable(con,'tluAudioLogger',tluAudioLogger.new)

### Reload tluAudioLogger
tluAudioLogger <- dbReadTable(con,'tluAudioLogger')

### Add AudioLoggerID value ####
dat <- dat %>% 
  mutate(`Detector Serial No.` = 
           case_when(is.na(`Detector Serial No.`) ~ 'Unknown',
                     TRUE ~ `Detector Serial No.`)) %>% 
  select(-AudioLoggerID) %>% 
  left_join(tluAudioLogger %>% 
              select('AudioLoggerID' = ID,
                     SerialNum,
                     Model),
            by = c('Detector Type' = 'Model',
                   'Detector Serial No.' = 'SerialNum'))

### Check to make sure all records have a tluAudioLogger relationship
dat %>% 
  distinct(AudioLoggerID) %>% 
  arrange(AudioLoggerID) %>% 
  print(n=Inf)


## DeploymentDate ####
dat <- dat %>% 
  mutate(DeploymentDate = as.Date(`Deployment Date`))

dat %>% 
  distinct(DeploymentDate) %>% 
  arrange(DeploymentDate) %>% 
  print(n=Inf)

## RecoveryDate ####
dat <- dat %>% 
  mutate(RecoveryDate = as.Date(`Recovery Date`))

dat %>% 
  distinct(RecoveryDate) %>% 
  arrange(RecoveryDate) %>% 
  print(n=Inf)

## RecordingStartDate ####
# This field seems unneccessary, since we have DeploymentDate and RecordingStartTime is a date/time object, but it's required in the database
dat <- dat %>% 
  mutate(RecordingStartDate = DeploymentDate)

## RecordingStartTime ####
# Must be date/time format
# read_excel set `Recording Start Time` at dttm data type, which assigns a date of 12/31/1899 to values that were only time originally
# Add local time zone to all Station Locations
dat <- dat %>% 
  st_as_sf(coords = c('Longitude','Latitude'),
           crs = 4269) %>% 
  mutate(tz_local = tz_lookup(.,method = 'accurate'),
         Latitude = st_coordinates(.)[,2],
         Longitude = st_coordinates(.)[,1]) %>% 
  st_drop_geometry()

dat %>% 
  distinct(FieldMaps)

dat <- dat %>% 
  mutate(RecordingStartTime =
           case_when(FieldMaps == F &
                       !is.na(`Recording Start Time`) ~ as.character(ymd_hm(paste(format(`Deployment Date`,'%Y-%m-%d'),format(`Recording Start Time`,'%H:%M')))),
                     FieldMaps == T &
                       tz_local == 'America/Boise' ~ as.character(with_tz(`Recording Start Time`,tzone = 'America/Boise')),
                     FieldMaps == T &
                       tz_local == 'America/Los_Angeles' ~ as.character(with_tz(`Recording Start Time`,tzone = 'America/Los_Angeles')),
                     TRUE ~ as.character(as.Date(`Deployment Date`) + hours(19) + minutes(30))))

dat %>% 
  filter(is.na(RecordingStartTime))
min(dat$RecordingStartTime)
max(dat$RecordingStartTime)
sort(str_extract(dat$RecordingStartTime,'\\d{2}:\\d{2}'))


## RecordingStopDate ####
# This field seems unneccessary, since we have RecoveryDate and RecordingStopTime is a date/time object
dat <- dat %>% 
  mutate(RecordingStopDate = RecoveryDate)

## RecordingStopTime ####
### Must be date/time format
dat <- dat %>% 
  mutate(RecordingStopTime =
           case_when(FieldMaps == F &
                       !is.na(`Recording Stop Time`) ~ as.character(ymd_hm(paste(format(`Recovery Date`,'%Y-%m-%d'),format(`Recording Stop Time`,'%H:%M')))),
                     FieldMaps == T &
                       tz_local == 'America/Boise' ~ as.character(with_tz(`Recording Stop Time`,tzone = 'America/Boise')),
                     FieldMaps == T &
                       tz_local == 'America/Los_Angeles' ~ as.character(with_tz(`Recording Stop Time`,tzone = 'America/Los_Angeles')),
                     TRUE ~ as.character(as.Date(RecoveryDate) + hours(6) + minutes(30))))

dat %>% 
  filter(is.na(RecordingStopTime))
min(dat$RecordingStopTime)
max(dat$RecordingStopTime)
dat %>% 
  mutate(tst1 = str_extract(RecordingStopTime,'\\d{2}:\\d{2}')) %>% 
  distinct(tst1) %>% 
  arrange(tst1) %>% 
  print(n=Inf)


## PrimaryContactID ####
# Organization
# Identify Organizations not in tluContact
dat %>% 
  filter(!`Deployment Agency` %in% (tluContact %>% 
                                     distinct(Organization) %>% 
                                     pull())) %>% 
  distinct(`Deployment Agency`)

# Investigate Agency = Other records
dat %>% 
  filter(`Deployment Agency` == 'Other') %>% 
  select(LocationName,`Deployment Agency`,`Enter other Agency`,`Deployment Contact(s)`) %>% 
  print(n=Inf)

# Format Organization to match tluContact
dat <- dat %>% 
  mutate(Organization =
           case_when(SampleUnitID == 129206 ~ 'United States Geological Survey',
                     `Deployment Agency` == 'IDFG' ~ 'Idaho Department of Fish and Game',
                     `Deployment Agency` == 'BLM' ~ 'Bureau of Land Management',
                     `Deployment Agency` == 'OSU' ~ 'Oregon State University-Cascades',
                     `Deployment Agency` == 'USFS' ~ 'US Forest Service',
                     `Deployment Agency` == 'NPS' ~ 'National Park Service',
                     `Deployment Agency` == 'Idaho Master Naturalist' ~ 'Volunteer',
                     `Deployment Agency` %in% c('Idaho National Laboratory','INL') ~ 'Veolia Nuclear Solutions',
                     `Deployment Agency` == 'ODFW' ~ 'Oregon Department of Fish and Wildlife',
                     `Deployment Agency` == 'WDFW' ~ 'Washington Department of Fish and Wildlife',
                     `Deployment Agency` == 'KTI' ~ 'Kalispel Tribe of Indians',
                     `Deployment Agency` == 'WPZ' ~ 'Woodland Park Zoo',
                     `Deployment Agency` == 'USGS' ~ 'United States Geological Survey',
                     `Deployment Agency` == 'USFWS' ~ 'US Fish and Wildlife Service',
                     `Enter other Agency` == 'TNC' ~ 'The Nature Conservancy',
                     `Enter other Agency` %in% c('Idaho Army National Guard','IDARNG') ~ 'Idaho Army National Guard',
                     TRUE ~ `Deployment Agency`))
dat %>% 
  filter(!Organization %in% (tluContact %>% 
                               distinct(Organization) %>% 
                               pull())) %>% 
  distinct(Organization)


# Read in CellTracker
cells <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2022.xlsx', sheet = '2022') %>% 
  filter(GRTS_ID != '14993') # Remove erroneous record of CONUS 99593, GRTS 14993


# Join CellTracker to dat
dat <- dat %>% 
  left_join(cells %>% 
              select(CONUS_10KM,Contact,Agency,Email), 
            by = c('SampleUnitID' = 'CONUS_10KM')) %>% 
  mutate(LastName = str_extract(Contact,'(?<=\\s).+$'),
         FirstName = str_extract(Contact,'^.+(?=\\s)'))

# Join tluContact by Organization (from metadata) and FirstName LastName (from CellTracker)
# Investigate all without matching tluContact records
dat %>% 
  left_join(tluContact %>% 
              select(ID,LastName,FirstName,Organization)) %>% 
  filter(is.na(ID) &
           Organization != 'Oregon State University-Cascades') %>% 
  distinct(SampleUnitID,`Deployment Contact(s)`,Organization,Contact) %>% 
  print(n=Inf)

dat <- dat %>% 
  mutate(Contact =
           case_when(SampleUnitID %in% c(126481,121390,124171) ~ 'Diane Probasco',
                     SampleUnitID %in% c(126024) ~ 'Jennifer Durbin',
                     SampleUnitID %in% c(98724,100112) ~ 'Bryan Bybee',
                     SampleUnitID %in% c(92224,92221) ~ 'Lyn Snoddy',
                     SampleUnitID %in% c(98689) ~ 'Rita Dixon',
                     SampleUnitID %in% c(97263,99117) ~ 'Tom Collom',
                     SampleUnitID %in% c(103301,103764,105159) ~ 'Rodney Klus',
                     SampleUnitID %in% c(99569,100041,100044) ~ 'Michael Moore',
                     SampleUnitID %in% c(101883) ~ 'Jade Keehn',
                     SampleUnitID %in% c(103755,105134,110232) ~ 'Andrew Walch',
                     SampleUnitID %in% c(104219,104679,105142,106536,106997) ~ 'Stephanie McKinney',
                     SampleUnitID %in% c(104648,105578,105579) ~ 'Amanda Cutler',
                     SampleUnitID %in% c(104665,106511) ~ 'Bill Cannaday',
                     SampleUnitID %in% c(105615,107925) ~ 'Greg Jackle',
                     SampleUnitID %in% c(107020,107481,109335,110726,113503) ~ 'Brian Ratliff',
                     SampleUnitID %in% c(110673) ~ 'Jason Kirchner',
                     SampleUnitID %in% c(110715) ~ 'Daniel Somers',
                     SampleUnitID %in% c(111154,113930) ~ 'Darren Bolen',
                     SampleUnitID %in% c(111657,112582) ~ 'Brehan Furfey',
                     SampleUnitID %in% c(112077) ~ 'Greg Reed',
                     SampleUnitID %in% c(112992) ~ 'Jason Kirchner',
                     SampleUnitID %in% c(113007,115786,119485) ~ 'Ben Cate',
                     SampleUnitID %in% c(113472,113478) ~ 'Andrew Meyers',
                     SampleUnitID %in% c(115307,116699,119480) ~ 'Paul Atwood',
                     SampleUnitID %in% c(120885,121346,122273,122276,130617) ~ 'Tara Chestnut',
                     SampleUnitID %in% c(123209,123670,124594,124599) ~ 'Janet Millard',
                     SampleUnitID %in% c(123680) ~ 'Daniel Misch',
                     Organization == 'Washington Department of Fish and Wildlife' ~ 'Abby Tobin',
                     LocationName %in% c('117633_NE1','117633_NW2','117633_SE1','117633_SW1') ~ "Ben Cate",
                     LocationName %in% c('117633_NW3','117633_NW4','117633_NW5','117633_NW6','117633_NW7','117633_NW8') ~ "Lauri Brewster",
                     TRUE ~ Contact),
         LastName = str_extract(Contact,'(?<=\\s).+$'),
         FirstName = str_extract(Contact,'^.+(?=\\s)'))

# Clean up Bat Hub records
dat %>% 
  filter(Organization == 'Oregon State University-Cascades') %>% 
  distinct(`Deployment Contact(s)`,Organization,Contact) %>% 
  print(n=Inf)

dat <- dat %>% 
  mutate(Contact =
           case_when(Organization == 'Oregon State University-Cascades' &
                       str_detect(`Deployment Contact(s)`,'[Br]own|[Mm]yers') ~ 'Erin Brown',
                     Organization == 'Oregon State University-Cascades' &
                       str_detect(`Deployment Contact(s)`,'[Pp]ierce|[Ss]trachan') ~ 'Adam Pierce',
                     Organization == 'Oregon State University-Cascades' &
                       str_detect(`Deployment Contact(s)`,'[Ss]chwartz|[Ll]orenz') ~ 'Megan Schwartz',
                     Organization == 'Oregon State University-Cascades' &
                       str_detect(`Deployment Contact(s)`,'[Cc]orrow') ~ 'Autumn Corrow',
                     Organization == 'Oregon State University-Cascades' &
                       str_detect(`Deployment Contact(s)`,'[Ee]mblidge') ~ 'Patrick Emblidge',
                     Organization == 'Oregon State University-Cascades' &
                       str_detect(`Deployment Contact(s)`,'[Hh]ernandez') ~ 'Mauro Hernandez',
                     TRUE ~ Contact),
         LastName = str_extract(Contact,'(?<=\\s).+$'),
         FirstName = str_extract(Contact,'^.+(?=\\s)'))

dat %>% 
  filter(FirstName=='Bat') %>% 
  select(LocationName,`Deployment Contact(s)`)

# Add PrimaryContactID from tluContact
dat <- dat %>% 
#  select(-PrimaryContactID) %>% 
  left_join(tluContact %>% 
              select(ID,LastName,FirstName,Organization)) %>% 
  rename('PrimaryContactID' = 'ID')

tluContact.new <- dat %>% 
  filter(is.na(PrimaryContactID)) %>% 
  distinct(Organization,LastName,FirstName)

# Check that these contacts aren't already in tluContacts
tluContact.new


# Add emails
tluContact.new <- tluContact.new %>% 
  mutate(EmailAddress = 
           case_when(LastName == 'Herner-Thogmartin' ~ 'Jennifer_Herner-Thogmartin@fws.gov',
                     LastName == 'Dixon' ~ 'molly_dixon@fws.gov',
                     LastName == 'Derusseau' ~ 'sabrina.derusseau@usda.gov'))


tluContact.new <- tluContact.new %>% 
  mutate(CreatedBy = 'P. Emblidge',
         LastModifiedBy = 'P. Emblidge')


options(odbc.batch_rows = 1)
#dbAppendTable(con,'tluContact',tluContact.new)


### join PrimaryContactID value from tluContact ####
tluContact <- dbReadTable(con,'tluContact')

dat <- dat %>% 
  select(-PrimaryContactID) %>% 
  left_join(tluContact %>% 
              select(ID,FirstName,LastName,Organization)) %>%
  rename('PrimaryContactID' = 'ID') %>% 
  select(-c(Organization,Contact,Agency,Email,LastName,FirstName))

dat %>% 
  distinct(PrimaryContactID) %>% 
  arrange(PrimaryContactID) %>% 
  print(n=Inf)

dat %>% 
  filter(is.na(PrimaryContactID)) %>% 
  select(SampleUnitID,`Deployment Agency`,`Deployment Contact(s)`)

## SpeciesGroupID ####
# tluSpeciesGroup
# This lookup table is not very clear, but comparing with species included in our classifiers, I believe the most accurate matches are below
# Western OR = 23
# Eastern OR = 21
# Western WA = 44
# Eastern WA = 46
# ID = 46
# Great Basin = 21
# These are not the most often used values previously
tblDeployment %>% 
  group_by(SpeciesGroupID) %>% 
  count() %>% 
  arrange(desc(n))

classifier <- read_csv('C:/Users/emblidgp/Box/HERS_Working/Bats/DataManager/Scripts/C10KClassifier.csv')

dat <- dat %>% 
  left_join(classifier %>% 
              select(CONUS_10KM,Classifier),by = c('SampleUnitID' = 'CONUS_10KM'))
dat %>% 
  distinct(Classifier)

dat %>% 
  filter(is.na(Classifier)) %>% 
  distinct(State)


dat <- dat %>% 
  mutate(SpeciesGroupID =
           case_when(SampleUnitID == 96343 ~ 21,
                     State == 'NV' ~ 21,
                     Classifier == 'Western Oregon' ~ 23,
                     Classifier == 'Eastern Oregon' ~ 21,
                     Classifier == 'Western Washington' ~ 44,
                     Classifier == 'Eastern Washington' ~ 46,
                     TRUE ~ 46)) %>% 
  select(-Classifier)

## Notes ####
# strings with length = 0 are not valid in database, must be NA
dat <- dat %>% 
  mutate(Notes = Comments)

### Remove BatHub Notes
dat %>% 
  distinct(Notes) %>% 
  print(n=Inf)

dat <- dat %>% 
  mutate(Notes = str_remove(Notes,'BatHub.+;\\s*'))
dat <- dat %>% 
  mutate(Notes = str_remove(Notes,'BatHub.+'))
#dat <- dat %>% 
#  mutate(Notes = str_remove(Notes,'Bat Hub.+'))
#dat <- dat %>% 
#  mutate(Notes = str_remove(Notes,'BatHUB.+'))

dat <- dat %>% 
  mutate(Notes =
           case_when(Notes == '' ~ NA_character_,
                     TRUE ~ Notes))

dat %>% 
  filter(str_detect(Notes,'^[Bb]at')) %>% 
  distinct(Notes) %>% 
  print(n=Inf)


## DistanceRangeID ####
# tluDistanceRange
#
tblDeployment %>% 
  group_by(DistanceRangeID) %>% 
  count() %>% 
  arrange(desc(n))

dat %>% 
  distinct(`Distance to Clutter (m)`) %>% 
  arrange(`Distance to Clutter (m)`) %>% 
  print(n=Inf)

dat <- dat %>% 
  mutate(DistanceRangeID =
           case_when(`Distance to Clutter (m)` < 5 ~ 1,
                     `Distance to Clutter (m)` == 5 ~ 7,
                     `Distance to Clutter (m)` > 5 &
                       `Distance to Clutter (m)` <= 10 ~ 4,
                     `Distance to Clutter (m)` > 10 &
                       `Distance to Clutter (m)` <= 20 ~ 5,
                     `Distance to Clutter (m)` > 20 &
                       `Distance to Clutter (m)` <= 50 ~ 6,
                     `Distance to Clutter (m)` > 50 &
                       `Distance to Clutter (m)` <= 75 ~ 8,
                     `Distance to Clutter (m)` > 75 &
                       `Distance to Clutter (m)` <= 100 ~ 3,
                     `Distance to Clutter (m)` > 100 ~ 2,
                     is.na(`Distance to Clutter (m)`) ~ 21))

dat %>% 
  distinct(`Distance to Clutter (m)`,DistanceRangeID) %>% 
  arrange(`Distance to Clutter (m)`) %>% 
  print(n=Inf)


## PhotoOfSite ####
tblDeployment %>% 
  distinct(PhotoOfSite)

# List all Photos
tmp1 <- system("powershell -command \"Get-ChildItem -Path \'C:\\Users\\emblidgp\\Box\\HERS_BatAcousticFiles\\NABat\\Oregon\\Raw\\2022\\SitePhotos\\\' -r -file | select name \"", intern = T)
tmp1 <- tmp1[str_detect(tmp1,'\\d+_[NS][EW]\\d')]
tmp1 <- str_extract(tmp1,'\\d+_[NS][EW]\\d')
tmp1 <- unique(tmp1)
tmp2 <- system("powershell -command \"Get-ChildItem -Path \'C:\\Users\\emblidgp\\Box\\HERS_BatAcousticFiles\\NABat\\Washington\\Raw\\2022\\SitePhotos\\\' -r -file | select name \"", intern = T)
tmp2 <- tmp2[str_detect(tmp2,'\\d+_[NS][EW]\\d')]
tmp2 <- str_extract(tmp2,'\\d+_[NS][EW]\\d')
tmp2 <- unique(tmp2)
tmp3 <- system("powershell -command \"Get-ChildItem -Path \'C:\\Users\\emblidgp\\Box\\HERS_BatAcousticFiles\\NABat\\Idaho\\Raw\\2022\\SitePhotos' -r -file | select name \"", intern = T)
tmp3 <- tmp3[str_detect(tmp3,'\\d+_[NS][EW]\\d')]
tmp3 <- str_extract(tmp3,'\\d+_[NS][EW]\\d')
tmp3 <- unique(tmp3)
tmp4 <- system("powershell -command \"Get-ChildItem -Path \'C:\\Users\\emblidgp\\Box\\HERS_BatAcousticFiles\\NABat\\USFWS\\Raw\\2022\\SitePhotos' -r -file | select name \"", intern = T)
tmp4 <- tmp4[str_detect(tmp4,'\\d+_[NS][EW]\\d')]
tmp4 <- str_extract(tmp4,'\\d+_[NS][EW]\\d')
tmp4 <- unique(tmp4)

dat <- dat %>% 
  mutate(PhotoOfSite =
           case_when(LocationName %in% tmp1 |
                       LocationName %in% tmp2 |
                       LocationName_GRTS %in% tmp3 |
                       LocationName %in% tmp4 ~ T,
                     TRUE ~ F))


## MicrophoneTypeID ####
# tluMicrophoneType

### Check existing microphones with detectors
tblDeployment %>% 
  left_join(tluAudioLogger %>% 
              select(ID,Model),
            by = c('AudioLoggerID' = 'ID')) %>% 
  distinct(Model,MicrophoneTypeID)


### Add tluMicrophoneType records to match what we use for NABat
### Existing records
### D500X External Mic and SM4BAT will be stand-ins for 'Pettersson D500x' and 'Wildlife Acoustics SMM-U2', respectively
### Wildlife Acoustics SMM-U2 is also used with SM3BAT

### Detectors present in the dataset
dat %>% 
  left_join(tluAudioLogger %>% 
              select(ID,Model),
            by = c('AudioLoggerID' = 'ID')) %>% 
  distinct(Model)

### New to add
# SMMiniBat = generic Omni-dir
# Anabat Express = generic Omni-dir
# Anabat Swift = TITLEY AnaBat Swift
# SM2BAT = SMX-U1

# Label field has maximum length 20 characters
#tmp1 <- data.frame(Label = c('SMX-U1','TITLEY AnaBat Swift','generic Omni-dir'))
#options(odbc.batch_rows = 1)
#dbAppendTable(con,'tluMicrophoneType',tmp1)


dat <- dat %>% 
  left_join(tluAudioLogger %>% 
              select(ID,Model),
            by = c('AudioLoggerID' = 'ID')) %>% 
  mutate(MicrophoneTypeID =
           case_when(Model == 'D500X' ~ 5,
                     Model == 'SM4BAT' |
                       Model == 'SM3BAT' ~ 7,
                     Model == 'SMMiniBat' |
                       Model == 'Anabat Express' ~ 10,
                     Model == 'Anabat Swift' ~ 9,
                     Model == 'SM2BAT' ~ 8)) %>% 
  select(-c(Model))

dat %>% 
  distinct(MicrophoneTypeID,`Detector Type`) %>% 
  arrange(MicrophoneTypeID) %>% 
  print(n=Inf)


## MicrophoneHeightOffGround ####
dat <- dat %>% 
  mutate(MicrophoneHeightOffGround = `Microphone Height (m)`)

dat %>% 
  distinct(MicrophoneHeightOffGround) %>% 
  arrange(MicrophoneHeightOffGround)

## MicrophoneOrientationID ####
tluOrientation
dat %>% 
  distinct(`Microphone Orientation`) %>% 
  print(n=Inf)

dat <- dat %>% 
  mutate(MicrophoneOrientationID =
           case_when(`Microphone Orientation` == 'N' ~ 2,
                     `Microphone Orientation` == 'E' ~ 1,
                     `Microphone Orientation` == 'S' ~ 5,
                     `Microphone Orientation` == 'W' ~ 8,
                     `Microphone Orientation` == 'NE' |
                       `Microphone Orientation` == 'NNE' |
                       `Microphone Orientation` == 'ENE' ~ 3,
                     `Microphone Orientation` == 'SE' |
                       `Microphone Orientation` == 'SSE' |
                       `Microphone Orientation` == 'ESE' ~ 6,
                     `Microphone Orientation` == 'SW' |
                       `Microphone Orientation` == 'S/SW' |
                       `Microphone Orientation` == 'WSW' ~ 7,
                     `Microphone Orientation` == 'NW' |
                       `Microphone Orientation` == 'NNW' |
                       `Microphone Orientation` == 'WNW' ~ 4,
                     `Microphone Orientation` == 'vertical' ~ 9,
                     TRUE ~ 10))

dat %>% 
  distinct(MicrophoneOrientationID) %>% 
  arrange(MicrophoneOrientationID)


## ClutterTypeID ####
# I think we need to keep this field simple, rather than really digging into the 'Other Clutter Type' field
tluClutterType
tblDeployment %>% 
  left_join(tluClutterType,
            by = c('ClutterTypeID' = 'ID')) %>% 
  group_by(ClutterTypeID,Label) %>% 
  count() %>% 
  arrange(desc(n))

dat %>% 
  distinct(`Clutter Type`)
dat %>% 
  distinct(`Enter Other Clutter Type`) %>% 
  print(n=Inf)


dat <- dat %>% 
  mutate(ClutterTypeID =
           case_when(str_detect(`Clutter Type`,'[Ww]ater') ~ 1,
                     str_detect(`Clutter Type`,'[Bb]uilding') ~ 2,
                     str_detect(`Clutter Type`, '[Vv]egetation') ~ 3,
                     str_detect(`Clutter Type`,'[Rr]ock') ~ 4,
                     str_detect(`Clutter Type`,'[Oo]ther') ~ 5,
                     TRUE ~ 23))

dat %>% 
  distinct(`Clutter Type`,ClutterTypeID)


## ClutterPercent ####
# Existing records use clutter category (1 = < 25%, 2 = 25 - 50%, etc.)
tblDeployment %>% 
  group_by(ClutterPercent) %>% 
  count() %>% 
  arrange(desc(n))

dat %>% 
  distinct(`Clutter Category`)

dat <- dat %>% 
  mutate(ClutterPercent = 
           case_when(is.na(`Clutter Category`) ~ '',
                     TRUE ~ as.character(`Clutter Category`)))

dat %>% 
  distinct(ClutterPercent)

## Detector settings not usually in metadata, use values from Field Manual and add to Comments field ####
# I don't like that this is in comments, as opposed to in the settings fields, starting to shift towards using the settings fields, however, it's difficult to align fields with original database

### Comments field should contain BatHub comments on the deployment, primarily UnusualOccurences
### DetectorSettings field should contain detector settings for this deployment

# Identify records with non-standard detector settings and use this in comments assignment
dat %>% 
  filter(if_any(40:50, ~ !is.na(.))) %>% 
  select(LocationName,`Detector Type`,40:50) %>%
  View()


dat <- dat %>% 
  left_join(tluAudioLogger %>% 
              select('AudioLoggerID' = ID,Model)) %>% 
  mutate(Comments = 
           case_when(Model == 'D500X' &
                       !is.na(SAMP.FREQ) ~ paste0('Detector Settings - Sampling Frequency: ',str_extract(SAMP.FREQ,'\\d+'),', PreTrigger: OFF, Recording Length: 5, HP-Filter: NO, Autorec: YES, Trigger Sensitivity: MED, Input Gain: ',`Input Gain`,', Trigger level: ',`Trig Level`,', Interval: 0'),
                     Model == 'D500X' &
                       !is.na(`Input Gain`) ~ paste0('Detector Settings - Sampling Frequency: 500, PreTrigger: OFF, Recording Length: 5, HP-Filter: NO, Autorec: YES, Trigger Sensitivity: MED, Input Gain: ',`Input Gain`,', Trigger level: ',`Trig Level`,', Interval: 0'),
                     Model == 'D500X' ~ 'Detector Settings - Sampling Frequency: 500, PreTrigger: OFF, Recording Length: 5, HP-Filter: NO, Autorec: YES, Trigger Sensitivity: MED, Input Gain: 45, Trigger level: 160, Interval: 0',
                     Model == 'SM4BAT' &
                       !is.na(`Trigger window length`) ~ paste0('Detector Settings - Gain - 12 dB, 16K High Filter: OFF, Sample Rate: 384, Min Duration 1.5 ms, Max Duration: None, Min Trig Freq: 7kHz, Trigger Level: 12 dB, Trigger Window: ',`Trigger window length`,' s, Max Length: ',`Maximum file length`,' seconds, Compression: NONE'),
                     Model == 'SM4BAT' &
                       !is.na(RecLen) ~ paste0('Detector Settings - Gain - 12 dB, 16K High Filter: OFF, Sample Rate: 384, Min Duration 1.5 ms, Max Duration: None, Min Trig Freq: 7kHz, Trigger Level: 12 dB, Trigger Window: 3 s, Max Length: ',RecLen,' seconds, Compression: NONE'),
                     Model == 'SM4BAT' ~ 'Detector Settings - Gain - 12 dB, 16K High Filter: OFF, Sample Rate: 384, Min Duration 1.5 ms, Max Duration: None, Min Trig Freq: 7kHz, Trigger Level: 12 dB, Trigger Window: 3 s, Max Length: 5 seconds, Compression: NONE')) %>% 
  select(-Model)


## Upload to database ####
dat <- dat %>% 
  mutate(CreatedBy = 'P. Emblidge',
         LastModifiedBy = 'P. Emblidge')

### Format
dat <- dat %>% 
  select(PointLocationID,        # Required
         AudioLoggerID,          # Required
         DeploymentDate,         # Required
         RecoveryDate,
         RecordingStartDate,     # Required
         RecordingStartTime,     # Required
         RecordingStopTime,
         PrimaryContactID,       # Required
         SpeciesGroupID,
         Notes,
         DistanceRangeID,
         PhotoOfSite,
         MicrophoneTypeID,
         MicrophoneHeightOffGround,
         MicrophoneOrientationID,
         ClutterTypeID,
         ClutterPercent,
         Comments,
         CreatedBy,              # Required
         LastModifiedBy)         # Required


options(odbc.batch_rows = 1)
dbAppendTable(con,'tblDeployment',dat)

########################################
