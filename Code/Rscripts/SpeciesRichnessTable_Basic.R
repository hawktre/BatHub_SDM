### We need to improve this script to show species richness by StationLocation/MonitoringNight

library(tidyverse)
library(readxl)

### Read in all Database.txt files from the project of interest and combine
# Identify the directories you need to pull Vetting Outputs from
# USFS Region04
#folders <- c('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/FS R4 Call Analysis/2022 NABat Data/Processed/R04VettedOutput')
# PNW and USFWS
#folders <- c('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/NABat/Oregon/Processed/2022/VettingOutput',
#  'C:/Users/emblidgp/Box/HERS_BatAcousticFiles/NABat/Washington/Processed/2022/VettingOutput',
#  'C:/Users/emblidgp/Box/HERS_BatAcousticFiles/NABat/Idaho/Processed/2022/VettingOutput',
#  'C:/Users/emblidgp/Box/HERS_BatAcousticFiles/NABat/Nevada/Processed/2022/VettedOutput')
# Crater Lake National Park
folders <- c('C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Crater Lake Bat Data/2022_Data/VettingOutput')

# Read in all files from the identified folders and combine
acoustic <- NULL
for(i in 1:length(folders)){
  setwd(folders[i])
  files <- list.files(full.names = T)
  tmp1 <- NULL
  for(ii in 1:length(files)){
    tmp2 <- read_tsv(files[ii], col_types = cols(.default = "c")) %>% 
      select(MonitoringNight,ParentDir,`Species Manual ID`,`User|ManualIDSpp2`,`User|Comments`)
    tmp1 <- rbind(tmp1,tmp2)
    }
  acoustic <- rbind(acoustic,tmp1)
}


acoustic %>% 
  distinct(`User|Comments`)


### If there are any ManualIDSpp2, add row to acoustic
acoustic %>% 
  distinct(`User|ManualIDSpp2`)


# Edit name of Manual ID field
acoustic <- acoustic %>% 
  rename('ManualID' = 'Species Manual ID') %>% 
  select(-c(`User|ManualIDSpp2`,`User|Comments`))

acoustic %>% 
  distinct(ManualID) %>% 
  print(n=Inf)


# Check that the following code only returns bat IDs
acoustic %>% 
  filter(str_detect(ManualID,'^[A-Z]{4}$') &
           ManualID %!in% c('HILO','NOID')) %>% 
  distinct(ManualID) %>% 
  arrange(ManualID) %>% 
  print(n=Inf)

# Make a list of bat species to include in species richness table
bats <- acoustic %>% 
  filter(str_detect(ManualID,'^[A-Z]{4}$') &
           ManualID %!in% c('HILO','NOID')) %>% 
  distinct(ManualID) %>% 
  arrange(ManualID) %>% 
  pull(ManualID)

# Filter dataset to only include confirmed bat species
acoustic <- acoustic %>% 
  mutate(Detected = 1) %>% 
  filter(ManualID %in% bats)


richness <- acoustic %>% 
  pivot_wider(id_cols = c(ParentDir,MonitoringNight), names_from = ManualID, values_from = Detected, values_fn = min, values_fill = 0, names_sort = T) %>% 
  arrange(ParentDir,MonitoringNight) %>% 
  rename('Site' = 'ParentDir')


write_csv(richness, 'C:/Users/emblidgp/Box/HERS_BatAcousticFiles/Crater Lake Bat Data/2022_Data/SppRichness_CRLA_2022.csv', na = '')

