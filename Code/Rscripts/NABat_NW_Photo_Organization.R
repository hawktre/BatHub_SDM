### This script is used to copy Site Photos from NW NABat Project into subdirectories corresponding to the correct state

library(tidyverse)
library(readxl)

setwd('C:/Users/emblidgp/Desktop/NABatPhotos2023/NW')

CellTracker <- read_excel('C:/Users/emblidgp/Box/HERS_Working/Bats/Coordination/NABat_CellTracker_2023.xlsx') %>% 
  select(CONUS_10KM,GRTS_ID,NABatProject,NABatState)

# List OR Sample Units
OR_SUs <- CellTracker %>% 
  filter(NABatProject == 'NW' &
           NABatState == 'OR') %>% 
  pull(CONUS_10KM)

# List WA Sample Units
WA_SUs <- CellTracker %>% 
  filter(NABatProject == 'NW' &
           NABatState == 'WA') %>% 
  pull(CONUS_10KM)

# Create objects of files for each state
files <- list.files()
filesOR <- files[str_extract(files,"^\\d+") %in% OR_SUs]
filesWA <- files[str_extract(files,"^\\d+") %in% WA_SUs]

# Check that all files have been assigned to either OR or WA
length(files) == length(filesOR) + length(filesWA)

# Identify files not assigned to either OR or WA
files[!(files %in% filesOR |
        files %in% filesWA)]

# Create directories for each state in NW NABat Project
dir.create('OR')
dir.create('WA')

# Copy OR files to state directory, and remove from Project directory
file.copy(filesOR,paste0("OR/",filesOR))
file.remove(filesOR)

# Copy WA files to state directory, and remove from Project directory
file.copy(filesWA,paste0("WA/",filesWA))
file.remove(filesWA)
