### This script is used to create a Species Richness table for the selected NABat Project, using the output from NABat_4_UploadPrep.R

# Install and load packages ----
if(!'pacman' %in% installed.packages()){
  install.packages('pacman')
}

pacman::p_load(tidyverse,
               readxl,
               lubridate)

# Identify yourself, the Project, and year of data to upload ----
onid <- readline(prompt = "Enter your ONID username, this is used to connect to Box: ")
project <- readline(prompt = "Enter the NABat Project corresponding to the acoustic data you want to prep: {NW | ID | USFWS | USFS_Region04}")
year <- readline(prompt = "Enter the NABat survey year you wish to check data from: ")

# Read in the clean acoustic data created in NABat_4_UploadPrep.R script
data <- read_csv(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/DataRequests/NABat_DataRequest_',year,'/Bulk_Stationary_Acoustic_Full_Template_',project,'_NABatZC_',year,'_reports.csv'))

#Format
#data <- data[-c(1:2),] %>% 
  #mutate(MonitoringNight = 
           #case_when(is.na(`Audio Recording Time`) ~ as.Date(ymd_hms(`Survey Start Time`)),
                     #TRUE ~ as.Date(ymd_hms(`Audio Recording Time`) - lubridate::hours(12)))) %>% 
data <- data[-c(1:2),] %>%   
  select('GRTS Cell Id',
         `Location Name`,
         MonitoringNight,
         ManualID,
         `Unusual Occurrences`)

data %>% 
  filter(is.na(MonitoringNight)) %>% 
  select(`Location Name`,ManualID,`Unusual Occurrences`)

# Convert Code6 to Code4
data <- data %>% 
  mutate(ManualID =
           case_when(ManualID == 'LASCIN' ~ 'LACI',
                     ManualID == 'LASNOC' ~ 'LANO',
                     ManualID == 'MYOCIL' ~ 'MYCI',
                     ManualID == 'MYOEVO' ~ 'MYEV',
                     ManualID == 'MYOLUC' ~ 'MYLU',
                     ManualID == 'MYOYUM' ~ 'MYYU',
                     ManualID == 'MYOCAL' ~ 'MYCA',
                     ManualID == 'EPTFUS' ~ 'EPFU',
                     ManualID == 'MYOTHY' ~ 'MYTH',
                     TRUE ~ ManualID))


# Check that the following code only returns bat IDs
data %>% 
  filter(str_detect(ManualID,'^[A-Z]{4}$')) %>% 
  distinct(ManualID) %>% 
  arrange(ManualID) %>% 
  print(n=Inf)

# Make a list of bat species to include in species richness table
bats <- data %>% 
  filter(str_detect(ManualID,'^[A-Z]{4}$')) %>% 
  distinct(ManualID) %>% 
  arrange(ManualID) %>% 
  pull(ManualID)

# Filter dataset to only include unique Location/Night/Species
data <- data %>% 
  mutate(ManualID =
           case_when(ManualID %in% bats ~ ManualID,
                     TRUE ~ NA_character_),
         Detected = 1) %>% 
  distinct()

data %>% 
  distinct(ManualID)

# Convert to species richness table
data.richness <- data %>% 
  pivot_wider(id_cols = c(`GRTS Cell Id`,`Location Name`,MonitoringNight,`Unusual Occurrences`), names_from = ManualID, values_from = Detected, values_fn = min, values_fill = 0, names_sort = T) %>% 
  arrange(`GRTS Cell Id`,`Location Name`)



# Add 'Agency'_Unit data from tracker (e.g., USFS_Unit, USFWS_Unit)
CellTracker<-read_excel(paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_',year,'.xlsx'))

if(project == 'ID'){
  data.richness <- data.richness %>% 
    left_join(CellTracker %>% 
                select('GRTS Cell Id' = GRTS_ID, USFS_Unit))
}

# Format Species Richness table
data.richness <- data.richness %>% 
  {if(project == "NW") select(.,`GRTS Cell Id`,`Location Name`,MonitoringNight,all_of(bats),`Unusual Occurrences`) else .} %>% 
  {if(project == "ID") select(.,USFS_Unit,`GRTS Cell Id`,`Location Name`,MonitoringNight,all_of(bats),`Unusual Occurrences`) else .} %>% 
  {if(project == "USFWS") select(.,USFWS_Unit,`GRTS Cell Id`,`Location Name`,MonitoringNight,all_of(bats),`Unusual Occurrences`) else .} %>% 
  {if(project == "USFS_Region04") select(.,USFS_Unit,`GRTS Cell Id`,`Location Name`,MonitoringNight,all_of(bats),`Unusual Occurrences`) else .}


write_csv(data.richness, paste0('C:/Users/',onid,'/Box/HERS_Working/Bats/Analysis_NABat/',year,'_Analysis/SppRichnessZC_',project,'_',year,'.csv'), na = '')










### Combine full spectrum and zero cross SppRich for ID
csv_file1 <- "C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/SppRichness_ID_2023.csv"
csv_file2 <- "C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/SppRichnessZC_ID_2023.csv"

acousticSB <- read.csv(csv_file1)
acousticZC <- read.csv(csv_file2)

combined_data <- dplyr::bind_rows(acousticSB, acousticZC)

write.csv(combined_data,"C:/Users/schminad/Box/HERS_Working/Bats/Analysis_NABat/2023_Analysis/SppRichnessCombined_ID_2023.csv" , row.names = FALSE)


