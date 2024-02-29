### Join Metadata to Acoustic Data and format for NABat upload

# Install and load packages ----
install.packages('pacman')

pacman::p_load(tidyverse,
               readxl,
               stringr,
               lubridate,
               lutz,
               DBI)

# Identify yourself, the Project, and year of data to upload ----
onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
project <- readline(prompt = "Enter the NABat Project corresponding to the acoustic data you want to prep: {NW | ID | USFWS | USFS_Region04}")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")

# Read in clean metadata from NABat_2_MetadataCleanup.R ----
meta <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABat_CleanMetadataTIMEFORMAT_',year,'.csv'), col_types = cols(.default = 'c'))

# Read in clean acoustic data from NABat_3_AcousticDataCleanup.R ----
#acoustic <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABat_CleanAcousticData_',project,'_',year,'.csv'),col_types = cols(.default = 'c'))

# For Idaho 2023, read in "Combined" clean acoustic data from NABat_3_AcousticDataCLeanup_ZeroCross.R 
  ## Had to format ZC and SB 'Audio Recording Time' separately then combine acoustic data. Also had to format 'survey start/end time' of NABat_CleanMetadata.
acoustic <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/NABat_CleanAcousticDataTIMEFORMAT_',project,'_',year,'.csv'),col_types = cols(.default = 'c'))

# Join metadata to acoustic data ----
import <- acoustic %>% 
  left_join(meta, by = c('GRTS Cell Id','Location Name','SiteDeployment'))


## Perform some preliminary checks ----
# Check to make sure there is no acoustic data without associated metadata
import %>% 
  filter(is.na(`Survey Start Time`)) %>% 
  distinct(`Location Name`)

# Check to make sure join didn't create duplicates
import %>% 
  group_by(`Audio Recording Name`) %>% 
  count() %>% 
  filter(n>1) %>% 
  distinct(str_extract(`Audio Recording Name`,'\\d+_[NS][EW]\\d'))


## Identify records where `Audio Recording Time` falls outside the deployment time ----
# Records marked as Exclude = 'Yes' will not be uploaded to NABat, so check to make sure these files should actually be excluded (i.e. Survey Start/End Times are correct)
import <- import %>% 
  mutate(Exclude = 
           case_when(`Audio Recording Time`  < `Survey Start Time` | `Audio Recording Time`  > `Survey End Time` ~ 'Yes',
                     TRUE ~ NA_character_))

# Investigate acoustic files outside of Survey Time
import %>% 
  filter(Exclude=='Yes') %>% 
  distinct(`Location Name`) %>% 
  print(n=Inf)

# Look up specific files outside of deployment time
import %>% 
  filter(`Location Name` == '1585_NE1' & Exclude == 'Yes') %>% 
  select(`Survey Start Time`, `Survey End Time`, `Audio Recording Time`, `Auto Id`, `Manual Id`) %>% 
  arrange(`Audio Recording Time`) %>% 
  print(n=Inf)

# Okay to exclude files outside of run time for the following deployments

# 218_SW1 - NoID files
# 3025_SW1 - one noise file excluded
# 96362_NE2
# 102386_NW1
# 102350_NE1
# 102351_SW1
# 102815_NW1
# 103755_NW1 and SE2 and SE3
# 104648_NE4
# 106536_SE1
# 107020_SE2     
# 107481_SE1     
# 107481_NE1     
# 107481_SW1
# 107925_SW1
# 109335_NW2
# 111154_NW1
# 125704_NE4
# 115350_SE3
# 115351_SE2     
# 117177_SW1     
# 117182_NE1     
# 117633_NE2 and SW1
# 118099_NE2
# 118131_SE1
# 119042_NW3
# 120896_SE1
# 122276_SE1
# 122276_NW1
# 122291_NW1
# 130617_SW1 
# 130594_SE1
# 11361_NE1
# 20817_SW1
# 1098_NE1       
# 12162_SW2
# 34753_SW1
# 64865_SW1
# 1242_SE2
# 138_NW1, NE1, SE1 - NoID files        
# 1610_SE2       
# 2081_NW3       
# 2378_SE1       52336 woke at 19:27
# 2578_NW3       52336 woke at 19:27
# 2593_NE1       52336 woke at 19:23
# 3034_NW2       52336 woke at 19:26
# 3210_SE1       52336 woke at 19:25
# 394_SE1        52336 woke at 19:26
# 4049_NW1       52336 woke at 19:23
# 4129_NE1       52336 woke at 19:22
# 4234_NE1       52336 woke at 19:27
# 4385_SW1       52336 woke at 19:24
# 4577_SE1
# 4682_NE1       52336 woke at 19:27
# 4938_NE2       52336 woke at 19:27
# 4913_SE2
# 5018_NW1 - Deployment failure
# 5314_SE1
# 5985_SW1 
# 721_SW1        52336 woke at 19:13

## Add Species List ----
# Read in ConusGrtsClassifier.csv from Box
classifier <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/DataManager/Scripts/ConusGrtsClassifier.csv'), col_types = cols(.default = 'c'))
#If USFS_Region04 ----
#classifier <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/DataManager/Scripts/ConusGrtsClassifier_USFSRegion4.csv'), col_types = cols(.default = 'c'))

# Format the classifier field names
classifier <- classifier %>% 
  select('GRTS Cell Id' = GRTS_ID,
         'Species List' = Classifier)

# Format classifier Species List to match NABat format
classifier <- classifier %>% 
  mutate(`Species List` = 
           case_when(str_detect(`Species List`,'^E') ~ str_replace(`Species List`,'^E','e'),
                     str_detect(`Species List`,'^W') ~ str_replace(`Species List`,'^W','w'),
                     TRUE ~ `Species List`))

# Check to make sure all Species List values are valid
classifier %>% 
  distinct(`Species List`)

# Add Classifier to import
import <- import %>% 
  left_join(classifier) %>% 
  mutate(`Species List` = paste0('SonoBat 4 ', `Species List`))

# Make sure that all data has valid Species List value
import %>% 
  distinct(`Species List`)


## Add a record for any Site surveyed that has no acoustic data ----
# Show all metadata records with no acoustic files
meta2 <- meta %>% 
  filter(NABatProject == project) %>% 
  anti_join(import,
            by = c('Location Name',
                   'SiteDeployment'))

meta2 %>% 
  select(`GRTS Cell Id`,`Location Name`,SiteDeployment,UnusualOccurrences) %>% 
  print(n=Inf)

##2023 Notes ##
# 98199_SE2 all files scrubbed as noise but settings look normal
# 126901_SW3 and NW1 all files scrubbed as noise but settings look normal

# Read in CellTracker
CellTracker <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_',year,'.xlsx')) %>% 
  select('GRTS Cell Id' = GRTS_ID,
         NABatProject)

# Read in VettingTracker
vetNotes <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_BatAcousticFiles/NABat/VettingTracker',year,'.xlsx'))

# Format VettingTracker to include only records related to the current project
vetNotes <- vetNotes %>% 
  left_join(CellTracker,
            by = c('GRTS' = 'GRTS Cell Id')) %>% 
  mutate('Location Name' = 
           case_when(project == 'NW' ~ LocationNameCONUS,
                     TRUE ~ paste0(GRTS,'_',str_extract(LocationNameCONUS,'[NS][EW]\\d'))),
         SiteDeployment = as.character(SiteDeployment)) %>% 
  filter(NABatProject == project)

### Locations with only Noise files ----
tmp1 <- vetNotes %>% 
  filter(filesRecorded > 0 &
           filesScrubbed == 0)

tmp1 %>% 
  select(`Location Name`,SiteDeployment) %>% 
  print(n=Inf)

# None should be in import
tmp1 %>% 
  filter(paste0(`Location Name`,'_',SiteDeployment) %in% paste0(import$`Location Name`,'_',import$SiteDeployment))

# Do any have UnusualOccurrences?
meta2 %>% 
  filter(paste0(`Location Name`,'_',SiteDeployment) %in% (tmp1 %>% 
                                                            mutate(check = paste0(`Location Name`,'_',SiteDeployment)) %>% 
                                                            pull(check))) %>% 
  select(`Location Name`,SiteDeployment,UnusualOccurrences) %>% 
  print(n=Inf)

# Add UnusualOccurrences
# It would be a good idea to make these additions in the original metadata and re-create NABat_CleanMetadata_{year}.csv
meta2 <- meta2 %>% 
  mutate(UnusualOccurrences =
           case_when(paste0(`Location Name`,'_',SiteDeployment) %in% (tmp1 %>% 
                                                                        mutate(check = paste0(`Location Name`,'_',SiteDeployment)) %>% 
                                                                        pull(check)) &
                       is.na(UnusualOccurrences) ~ 'All acoustic files classified as Noise',
                     TRUE ~ UnusualOccurrences))


### Failed deployments ----
tmp2 <- vetNotes %>% 
  filter(filesRecorded == 0)

tmp2 %>% 
  select(`Location Name`,SiteDeployment) %>% 
  print(n=Inf)

# None should be in import
tmp2 %>% 
  filter(paste0(`Location Name`,'_',SiteDeployment) %in% paste0(import$`Location Name`,'_',import$SiteDeployment))

# Do any have UnusualOccurrences?
meta2 %>% 
  filter(paste0(`Location Name`,'_',SiteDeployment) %in% (tmp2 %>% 
                                                            mutate(check = paste0(`Location Name`,'_',SiteDeployment)) %>% 
                                                            pull(check))) %>% 
  select(`Location Name`,SiteDeployment,UnusualOccurrences) %>% 
  print(n=Inf)

# Add UnusualOccurrences
# It would be a good idea to make these additions in the original metadata and re-create NABat_CleanMetadata_{year}.csv
meta2 <- meta2 %>% 
  mutate(UnusualOccurrences =
           case_when(paste0(`Location Name`,'_',SiteDeployment) %in% (tmp2 %>% 
                                                                        mutate(check = paste0(`Location Name`,'_',SiteDeployment)) %>% 
                                                                        pull(check)) &
                       is.na(UnusualOccurrences) ~ 'Deployment failure',
                     TRUE ~ UnusualOccurrences))

### Check for other reasons for no acoustic data ----
meta2 %>% 
  filter(!`Location Name` %in% c(tmp1$`Location Name`,tmp2$`Location Name`)) %>% 
  select(`Location Name`,SiteDeployment,UnusualOccurrences)

# Investigate each deployment with no acoustic data not previously attributed
meta2 <- meta2 %>% 
  mutate(UnusualOccurrences =
           case_when(`Location Name` == '110673_NW2' & SiteDeployment == 1 ~ 'Detector failure - no acoustic data',
                     `Location Name` == '105578_NW1' & SiteDeployment == 1 ~ 'Detector failure - no acoustic data',
                     TRUE ~ UnusualOccurrences))

### Add records for deployments with no acoustic data ----
import <- import %>% 
  add_row(meta2)


# Format and Save output ----
# Read in NABat Bulk Stationary Upload Template from Box
template <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/DataRequests/Bulk_Stationary_Acoustic_Full_Template.xlsx'))

# Add acoustic records to template
template <- template %>% 
  add_row(import %>% 
            filter(is.na(Exclude)) %>% 
            mutate(across(everything(), as.character)) %>% 
            select('| GRTS Cell Id' = `GRTS Cell Id`,
                   `Location Name`,
                   # Idaho and USFS upload include Lat/Long, NW NABat don't include
                   Latitude,
                   Longitude,
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
                   `Contact`,
                   `Audio Recording Name`,
                   `Audio Recording Time`,
                   `Software Type`,
                   `Auto Id`,
                   `Manual Id`,
                   `Species List`,
                   'Unusual Occurrences' = UnusualOccurrences))

# Save prepared data to your local computer
# These files are too large to write to Box efficiently
write_csv(template, paste0('C:/Users/',onid,'/Desktop/Bulk_Stationary_Acoustic_Full_Template_',project,'_NABat_',year,'.csv'), na = "")

# Back up this output file in Box\HERS_Working\Bats\DataRequests\NABat_DataRequest_{year}
# Upload to NABat Partner Portal
############################################################################################################################################################

### Combine zero cross data
csv_file1 <- "C:/Users/schminad/Box/HERS_Working/Bats/DataRequests/NABat_DataRequest_2023/Bulk_Stationary_Acoustic_Full_Template_1569_ZC_ID_NABat_2023.csv"
csv_file2 <- "C:/Users/schminad/Box/HERS_Working/Bats/DataRequests/NABat_DataRequest_2023/Bulk_Stationary_Acoustic_Full_Template_33_ZC_ID_NABat_2023.csv"
csv_file3 <- "C:/Users/schminad/Box/HERS_Working/Bats/DataRequests/NABat_DataRequest_2023/Bulk_Stationary_Acoustic_Full_Template_289_ZC_ID_NABat_2023.csv"
csv_file4 <- "C:/Users/schminad/Box/HERS_Working/Bats/DataRequests/NABat_DataRequest_2023/Bulk_Stationary_Acoustic_Full_Template_545_ZC_ID_NABat_2023.csv"

acoustic1 <- read.csv(csv_file1)
acoustic2 <- read.csv(csv_file2)
acoustic3 <- read.csv(csv_file3)
acoustic4 <- read.csv(csv_file4)

combined_data <- dplyr::bind_rows(acoustic1, acoustic2, acoustic3, acoustic4)

write.csv(combined_data,"C:/Users/schminad/Box/HERS_Working/Bats/DataRequests/NABat_DataRequest_2023/Bulk_Stationary_Acoustic_Full_Template_ZC_ID__NABat_2023.csv" , row.names = FALSE)


# Format and Save output ----
# Read in NABat Bulk Stationary Upload Template from Box
template <- read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/DataRequests/Bulk_Stationary_Acoustic_Full_Template.xlsx'))
acousticZC <- read.csv(paste0('C:/Users/schminad/Box/HERS_Working/Bats/DataRequests/NABat_DataRequest_2023/Bulk_Stationary_Acoustic_Full_Template_ZC_ID__NABat_20230213.csv'))

# Add acoustic records to template
template <- template %>% 
  add_row(acousticZC %>% 
            filter(is.na(Exclude)) %>% 
            mutate(across(everything(), as.character)) %>% 
            select('| GRTS Cell Id' = `GRTS Cell Id`,
                   `Location Name`,
                   # Idaho and USFS upload include Lat/Long, NW NABat don't include
                   Latitude,
                   Longitude,
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
                   `Contact`,
                   `Audio Recording Name`,
                   `Audio Recording Time`,
                   `Software Type`,
                   `Auto Id`,
                   `Manual Id`,
                   `Species List`,
                   'Unusual Occurrences' = UnusualOccurrences))

# Save prepared data to your local computer
# These files are too large to write to Box efficiently
write_csv(template, paste0('C:/Users/',onid,'/Desktop/Bulk_Stationary_Acoustic_Full_Template_',project,'_NABat_ZC_',year,'.csv'), na = "")

