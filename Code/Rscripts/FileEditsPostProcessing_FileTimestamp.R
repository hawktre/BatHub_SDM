### This script provides tools for editing file timestamp because detector clock was wrong.
### If you run this script following file attributing, and before auto-classifying, you will only need to edit Processed File Name
### See FileEditsPostProcessing.R for additional tools to edit Station Location Name or Timestamp in files that have been auto-classified and Vetted

# Install and load packages ----
if(!'pacman' %in% installed.packages()){
  install.packages('pacman')
}

pacman::p_load(tidyverse,
               readxl,
               lubridate,
               sjmisc)

# Read in the files to edit ----
# Edit this line to read the directory of interest
setwd('E:/NABat/Idaho/Processed/2023/5153/5153_SW1')
files <- list.files()

# Adjust timestamp of filenames ----
# Edit this code chunk to add/subtract the desired amount of time
files2 <- str_replace(files,
                      "\\d{8}_\\d{6}",
                      format(ymd_hms(str_extract(files,
                                                 '\\d{8}_\\d{6}'))
                             + years(0) + months(0) + days(0) + hours(0) + minutes(0),"%Y%m%d_%H%M%S"))

# Check if there's any overlap in 'to' and 'from' files
files[files%in%files2]

# If no overlap, rename files
#file.rename(files,files2)

# If there's overlap between 'to' and 'from' filenames, we need to institute an intermediate step
# intermediate rename to ensure no overlap in 'to' and 'from' filenames
files3 <- as.character(1:length(files))
file.rename(files,files3)

# Rename files with final timestamp
file.rename(files3,files2)

# confirm we didn't lose any files
files4 <- list.files()
if(length(files) != length(files4)){
  warning("File rename overwrote some original files")
}

### Create ReadME file
### Transfer edited files to Box
